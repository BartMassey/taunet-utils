{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- Copyright © 2015 Bart Massey
-- [This program is licensed under the GPL version 3 or later.]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- TauNet message utilities

-- | This module contains various functions for generating
-- and parsing messages, as well as some associated logging
-- functionality.
module TaunetMessage (
  -- * Messages
  Message(..), Failure(..), FailFunc,
  generateMessage, showMessage, 
  -- * Message Parsing  
  Knobs(..), getKnobs, parseMessage,
  -- * Logging
  logString, logMessage )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.List
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import System.Console.ParseArgs
import System.IO
import System.Posix.Process
import Text.Printf

import DateTime
import LocalAddr
import TaunetUtil

data Command = CommandReplies Int | CommandDelay Double
             deriving Show

-- | 'Knobs' are parsed out of the commands in a `taunet-echo`
-- request message.
data Knobs = Knobs {
       knobReplies :: Int, -- ^ Number of times to reply to the echo request.
       knobDelay :: Double -- ^ Number of seconds to delay before each reply.
     }

-- | 'Message' record for sending and receiving. The
-- 'messageCommands' field is an internal-use-only kludge
-- used for command parsing. There should probably be a
-- typeclass here.
data Message = Message {
       messageTo, messageFrom :: String, -- ^ Source and destination.
       messageCommands :: [Command], -- ^ Internal field.
       messageBody :: BS.ByteString  -- ^ Contains just message body
                                     -- when sending, but full message
                                     -- text on receipt. This should be
                                     -- fixed.
    } deriving Show

-- | A 'Failure' record is used to record a failure and its reasons.
data Failure = Failure {
       failureMessage :: String, -- ^ Reason for failure.
       failureBody :: BS.ByteString -- ^ Entire failed message, I think.
    } deriving Show

type FailFunc = String -> forall a . Either Failure a

defaultKnobs :: Knobs
defaultKnobs = Knobs {
                 knobReplies = 1,
                 knobDelay = 0.1 }

-- | Process parsed commands to get values for the knobs.
-- Default values are one reply and 0.1 seconds delay.
getKnobs :: Either Failure Message -> Knobs
getKnobs (Right (Message { messageCommands = commands})) =
    foldl' handleCommand defaultKnobs commands
    where
      handleCommand knobs (CommandReplies n) =
          knobs {knobReplies = n}
      handleCommand knobs (CommandDelay d) =
          knobs {knobDelay = d}
getKnobs _ = defaultKnobs

failVal :: BS.ByteString -> FailFunc
failVal plaintext msg = Left $ Failure msg plaintext

readValue :: Read a => String -> FailFunc -> Either Failure a
readValue s failure =
    case reads s of
      [(v, "")] -> return v
      _ -> failure $ "bad value " ++ s

parseCommand :: [String] -> FailFunc -> Either Failure Command
parseCommand ["replies", countString] failure = do
    count <- readValue countString failure :: Either Failure Int
    failUnless (count >= 0) $
        failure "cannot repeat negative times"
    failUnless (count <= 10) $
        failure "will not repeat more than 10 times"
    return $ CommandReplies count
parseCommand ["delay", secondsString] failure = do
    seconds <- readValue secondsString failure :: Either Failure Double
    failUnless (seconds >= 0) $
        failure "cannot delay negative seconds"
    failUnless (seconds <= 60) $
        failure "will not delay more than 60 seconds"
    return $ CommandDelay seconds
parseCommand [] failure =
    failure $ "missing command"
parseCommand (commandString : _) failure =
    failure $ "bad command " ++ commandString

parseCommands :: [String] -> FailFunc
              -> Either Failure [Command]
parseCommands (('@' : commandString) : body) failure = do
    command <- parseCommand (words commandString) failure
    commands <- parseCommands body failure
    return $ command : commands
parseCommands _ _ = return []

removeHeader :: String -> String -> FailFunc -> Either Failure String
removeHeader header target failure
    | isPrefixOf paddedHeader target =
        return $ drop (length paddedHeader) target
    | otherwise =
        failure $ "bad header " ++ header
    where
      paddedHeader = header ++ ": "

-- | Process a raw message, dealing with header parsing and
-- command parsing.
parseMessage :: Bool -> BS.ByteString -> Either Failure Message
parseMessage _ plaintext
    | BS.length plaintext > maxMessageSize =
        Left $ Failure "message too long" plaintext
parseMessage processCommands plaintext = do
  let (headers, body) = splitAt 4 $ linesCRLF $ BSC.unpack plaintext
  let failure msg = failVal plaintext msg
  failUnless (length headers == 4) $
             failure "mangled headers"
  failUnless (headers !! 3 == "") $
             failure "bad end-of-headers"
  versionHeader <- removeHeader "version" (headers !! 0) failure
  failUnless (versionHeader == versionNumber) $
             failure "bad version header"
  fromHeader <- removeHeader "from" (headers !! 1) failure
  failUnless (validUsername fromHeader) $
             failure "illegal from username"
  toHeader <- removeHeader "to" (headers !! 2) failure
  failUnless (validUsername toHeader) $
             failure "illegal to username"
  failUnless (BS.length plaintext <= maxMessageSize) $
             failure "overlong message"
  commands <- if processCommands
              then parseCommands body failure
              else return []
  return $ Message toHeader fromHeader commands plaintext
  where
    validUsername name =
        let nName = length name in
        nName >= 3 && nName <= 30 &&
        all validChar name
        where
          validChar c = isAlpha c || isDigit c || c == '-'

-- | Format up a message and send it using 'TaunetUtils.sendMessage'.
-- Not well named.
generateMessage :: Maybe BS.ByteString -> Maybe (String, SockAddr)
                -> AddressData -> Message
                -> IO ()
generateMessage maybeKey maybeStamp dest message = do
  let plaintext =
        ("version: 0.2\r\n" +++
        "from: " +++ fromPerson +++ "\r\n" +++
        "to: " +++ toPerson +++ "\r\n" +++
        "\r\n" +++
        makeStamp +++
        messageBody message) :: BSC.ByteString
        where
          fromPerson = BSC.pack $ messageFrom message
          toPerson = BSC.pack $ messageTo message
  let sa = portAddr taunetPort dest
  sendMessage maybeKey sa $ BS.take maxMessageSize plaintext
  where
    makeStamp =
        case maybeStamp of
          Nothing -> ""
          Just (r, a) ->
              BSC.pack r +++ " " +++ BSC.pack (show a) +++ "\r\n"

-- | Write a date and pid stamp and the given message
-- to the log file "echo.log" in the current directory.
--
-- Opens the log file on each message for
-- poor-person's synchronization: this should be fixed.
logString :: String -> IO ()
logString msg = do
  dateStr <- getTimeRFC3339
  pid <- getProcessID
  hLog <- openFile "echo.log" AppendMode
  _ <- hPrintf hLog "%s: %s: %s\n" dateStr (show pid) msg
  hClose hLog

-- | Log sender and recipient info for a message to
-- the log file.
logMessage :: AddressData -> Either Failure Message -> IO ()
logMessage address msg = do
  let addressStr = show address
  case msg of
    Right (Message { messageFrom = from, messageTo = to }) -> do
      logString $ printf "%s (%s) -> %s" from addressStr to
    Left (Failure { failureMessage = failure }) ->
      logString $ printf "(%s) -> : %s" addressStr failure

-- | Format a message for printing. Strips the headers off
-- the body first, which should be fixed by not having them
-- there in the first place.
showMessage :: Message -> String
showMessage m =
    (printf "from: %s\n" (messageFrom m)) ++
    (printf "to: %s\n\n" (messageTo m)) ++
    body
    where
      -- XXX This is gross: we should remember the body from
      -- parseMessage, but that makes things uglier.
      body = unlines $ tail $ dropWhile (not . null) $
             linesCRLF $ BSC.unpack $ messageBody m
