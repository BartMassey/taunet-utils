{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- Copyright © 2015 Bart Massey
-- [This program is licensed under the GPL version 3 or later.]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- TauNet echo server

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

data Message = Message { messageTo, messageFrom :: String,
                         messageBody :: BS.ByteString }
             deriving Show

data Failure = Failure { failureMessage :: String,
                         failureBody :: BS.ByteString }
             deriving Show

parseMessage :: BS.ByteString -> IO (Either Failure Message)
parseMessage plaintext = do
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
    fromHeader <- removeHeader "from" $ headers !! 1
    failUnless (validUsername fromHeader) $
               failure "illegal from username"
    toHeader <- removeHeader "to" $ headers !! 2
    failUnless (validUsername toHeader) $
               failure "illegal to username"
    failUnless (BS.length plaintext <= maxMessageSize) $
               failure "overlong message"
    return $ Message toHeader fromHeader plaintext
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
      let ha = hostAddr recvAddr
      recvTime <- getTimeRFC3339
      plaintext <- receiveMessage maybeKey recvSocket
      when (BS.length plaintext == 0) $ do
        logString $ printf
                      "(%s) ping (zero-length) message received and discarded"
                      (show ha)
        exitSuccess
      received <- parseMessage plaintext
      when debug $ print received
      logMessage ha received
      localAddresses <- getLocalAddresses
      when (ha `elem` localAddresses) exitSuccess
      let sendIt = generateMessage maybeKey recvTime recvAddr
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
