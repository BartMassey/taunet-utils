{-# LANGUAGE OverloadedStrings, BangPatterns, RankNTypes #-}
-- Copyright © 2015 Bart Massey
-- [This program is licensed under the GPL version 3 or later.]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- TauNet echo server

import Control.Concurrent.Thread.Delay
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.List
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import System.Console.ParseArgs
import System.Exit
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

parseMessage :: BS.ByteString -> Either Failure Message
parseMessage plaintext | BS.length plaintext > maxMessageSize =
    Left $ Failure "message too long" plaintext
parseMessage plaintext = do
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
  commands <- parseCommands body failure
  return $ Message toHeader fromHeader commands plaintext
  where
    validUsername name =
        let nName = length name in
        nName >= 3 && nName <= 30 &&
        all validChar name
        where
          validChar c = isAlpha c || isDigit c || c == '-'

generateMessage :: Maybe BS.ByteString -> String -> SockAddr -> Message
                -> IO ()
generateMessage maybeKey recvTime sendAddr message = do
  let plaintext =
        ("version: 0.2\r\n" +++
        "from: " +++ fromPerson +++ "\r\n" +++
        "to: " +++ toPerson +++ "\r\n" +++
        "\r\n" +++
        BSC.pack recvTime +++ " " +++ BSC.pack (show sendAddr) +++ "\r\n" +++
        messageBody message) :: BSC.ByteString
        where
          toPerson = BSC.pack $ messageFrom message
          fromPerson = BSC.pack $ messageTo message
  let sa = portAddr taunetPort (hostAddr sendAddr)
  sendMessage maybeKey sa $ BS.take maxMessageSize plaintext

-- XXX Open the log file on each message for
-- poor-person's synchronization.
logString :: String -> IO ()
logString msg = do
  dateStr <- getTimeRFC3339
  pid <- getProcessID
  hLog <- openFile "echo.log" AppendMode
  hPrintf hLog "%s: %s: %s\n" dateStr (show pid) msg
  hClose hLog

logMessage :: AddressData -> Either Failure Message -> IO ()
logMessage address msg = do
  let addressStr = show address
  case msg of
    Right (Message { messageFrom = from, messageTo = to }) -> do
      logString $ printf "%s (%s) -> %s" from addressStr to
    Left (Failure { failureMessage = failure }) ->
      logString $ printf "(%s) -> : %s" addressStr failure

reapChildren :: IO ()
reapChildren = do
  maybeChild <- getAnyProcessStatus False True
  case maybeChild of
    Nothing -> return ()
    Just (pid, status) -> do
      when (status /= Exited ExitSuccess) $
           logString $ printf "%s exited prematurely: %s"
                         (show pid) (show status)
      reapChildren

data ArgIndex = ArgPlain | ArgDebug | ArgFailUser
              deriving (Eq, Ord, Enum, Show)

argd :: [ Arg ArgIndex ]
argd = [ Arg {
           argIndex = ArgPlain,
           argAbbr = Just 'p',
           argName = Just "plain",
           argData = Nothing,
           argDesc = "Do not encrypt the incoming or outgoing messages." },
         Arg {
           argIndex = ArgDebug,
           argAbbr = Just 'd',
           argName = Just "debug",
           argData = Nothing,
           argDesc = "Do debugging things." },
         Arg {
           argIndex = ArgFailUser,
           argDesc = "Specify TauNet username used in failure reports.",
           argAbbr = Just 'f',
           argName = Just "fail-user",
           argData = argDataDefaulted "username-prefix"
                     ArgtypeString "FAIL" } ]

main :: IO ()
main = do
  argv <- parseArgsIO ArgsComplete argd
  let !encrypted = not $ gotArg argv ArgPlain
  let !debug = gotArg argv ArgDebug
  let !failUser = getRequiredArg argv ArgFailUser
  maybeKey <- maybeGetKey encrypted
  taunetSocket <- socket AF_INET Stream defaultProtocol
  setSocketOption taunetSocket ReuseAddr 1
  bind taunetSocket $ SockAddrInet taunetPort 0
  listen taunetSocket 1
  forever $ do
    (recvSocket, recvAddr) <- accept taunetSocket
    _ <- forkProcess $ do
      close taunetSocket
      let ha = hostAddr recvAddr
      recvTime <- getTimeRFC3339
      plaintext <- receiveMessage
                     (Just (2 * maxMessageSize))
                     maybeKey
                     recvSocket
      when (BS.length plaintext == 0) $ do
        logString $ printf
                      "(%s) ping (zero-length) message received and discarded"
                      (show ha)
        exitSuccess
      let received = parseMessage plaintext
      let knobs = getKnobs received
      when debug $ do print received
      logMessage ha received
      localAddresses <- getLocalAddresses
      when (ha `elem` localAddresses) exitSuccess
      let replyMessage =
              case received of
                Right message -> message
                Left failure ->
                    Message {
                      messageTo = failUser ++ "-TO",
                      messageFrom = failUser ++ "-FROM",
                      messageCommands = [],
                      messageBody =
                          BSC.pack (failureMessage failure) +++
                                   "\r\n" +++ failureBody failure }
      replicateM_ (knobReplies knobs) $ do
        delay $ floor $ 1000000 * knobDelay knobs
        generateMessage maybeKey recvTime recvAddr replyMessage
      exitSuccess
    reapChildren
