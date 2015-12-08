{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- Copyright © 2015 Bart Massey
-- [This program is licensed under the GPL version 3 or later.]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- TauNet message utilities

module TaunetMessage (
  Knobs(..), Message(..), Failure(..), FailFunc,
  getKnobs, parseMessage, generateMessage,
  logString, logMessage, showMessage )
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

data Knobs = Knobs {
      knobReplies :: Int,
      knobDelay :: Double }

data Message = Message { messageTo, messageFrom :: String,
                         messageCommands :: [Command],
                         messageBody :: BS.ByteString }
             deriving Show

data Failure = Failure { failureMessage :: String,
                         failureBody :: BS.ByteString }
             deriving Show

type FailFunc = String -> forall a . Either Failure a

defaultKnobs :: Knobs
defaultKnobs = Knobs {
                 knobReplies = 1,
                 knobDelay = 0.1 }

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
          toPerson = BSC.pack $ messageFrom message
          fromPerson = BSC.pack $ messageTo message
  let sa = portAddr taunetPort dest
  sendMessage maybeKey sa $ BS.take maxMessageSize plaintext
  where
    makeStamp =
        case maybeStamp of
          Nothing -> ""
          Just (r, a) ->
              BSC.pack r +++ " " +++ BSC.pack (show a) +++ "\r\n"

-- XXX Open the log file on each message for
-- poor-person's synchronization.
logString :: String -> IO ()
logString msg = do
  dateStr <- getTimeRFC3339
  pid <- getProcessID
  hLog <- openFile "echo.log" AppendMode
  _ <- hPrintf hLog "%s: %s: %s\n" dateStr (show pid) msg
  hClose hLog

logMessage :: AddressData -> Either Failure Message -> IO ()
logMessage address msg = do
  let addressStr = show address
  case msg of
    Right (Message { messageFrom = from, messageTo = to }) -> do
      logString $ printf "%s (%s) -> %s" from addressStr to
    Left (Failure { failureMessage = failure }) ->
      logString $ printf "(%s) -> : %s" addressStr failure

showMessage :: Message -> String
showMessage m =
    (printf "from: %s\n" (messageFrom m)) ++
    (printf "to: %s\n\n" (messageTo m)) ++
    (BSC.unpack $ messageBody m)
