{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- Copyright © 2015 Bart Massey
-- [This program is licensed under the GPL version 3 or later.]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- TauNet echo server

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
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

data Message = Message { messageTo, messageFrom :: String,
                         messageBody :: BS.ByteString }
             deriving Show

data Failure = Failure { failureMessage :: String,
                         failureBody :: BS.ByteString }
             deriving Show

parseMessage :: Maybe BS.ByteString -> Socket
             -> IO (Either Failure Message)
parseMessage maybeKey s = do
  plaintext <- receiveMessage maybeKey s
  let headers = take 4 $ linesCRLF $ BSC.unpack plaintext
  return $ do
    let failure msg = Failure msg plaintext
    let removeHeader header target
            | isPrefixOf paddedHeader target =
                Right $ drop (length paddedHeader) target
            | otherwise =
                Left $ Failure ("bad header " ++ header) plaintext
            where
              paddedHeader = header ++ ": "
    failUnless (length headers == 4) $
               failure "mangled headers"
    failUnless (headers !! 3 == "") $
               failure "bad end-of-headers"
    versionHeader <- removeHeader "version" $ headers !! 0
    failUnless (versionHeader == versionNumber) $
               failure "bad version header"
    toHeader <- removeHeader "to" $ headers !! 1
    fromHeader <- removeHeader "from" $ headers !! 2
    return $ Message toHeader fromHeader plaintext

generateMessage :: Maybe BS.ByteString -> SockAddr -> Message -> IO ()
generateMessage maybeKey sendAddr message = do
  let plaintext =
        ("version: 0.2\r\n" +++
        "to: " +++ toPerson +++ "\r\n" +++
        "from: " +++ fromPerson +++ "\r\n\r\n" +++
        messageBody message) :: BSC.ByteString
        where
          toPerson = BSC.pack $ messageFrom message
          fromPerson = BSC.pack $ messageTo message
  sendMessage maybeKey sendAddr plaintext

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
      received <- parseMessage maybeKey recvSocket
      when debug $ print received
      let ha = hostAddr recvAddr
      logMessage ha received
      localAddresses <- getLocalAddresses
      when (ha `elem` localAddresses) exitSuccess
      let sendIt = generateMessage maybeKey $ portAddr taunetPort ha
      case received of
        Right message -> sendIt message
        Left failure -> sendIt failMessage
                   where
                     failMessage = Message {
                       messageTo = failUser ++ "-TO",
                       messageFrom = failUser ++ "-FROM",
                       messageBody =
                           BSC.pack (failureMessage failure) +++
                           "\r\n" +++ failureBody failure }
      exitSuccess
    reapChildren
