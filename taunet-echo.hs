{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- Copyright © 2015 Bart Massey
-- [This program is licensed under the GPL version 3 or later.]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- TauNet echo server

import Control.Concurrent.Thread.Delay
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import System.Console.ParseArgs
import System.Exit
import System.Posix.Process
import Text.Printf

import DateTime
import LocalAddr
import TaunetUtil
import TaunetMessage

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
  logString "taunet-echo begins"
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
      let received = parseMessage True plaintext
      let knobs = getKnobs received
      when debug $ do print received
      logMessage ha received
      localAddresses <- getLocalAddresses
      when (ha `elem` localAddresses) exitSuccess
      let replyMessage =
              case received of
                Right message -> swapFromTo message
                Left failure ->
                    Message {
                      messageTo = failUser ++ "-TO",
                      messageFrom = failUser ++ "-FROM",
                      messageCommands = [],
                      messageBody =
                          BSC.pack (failureMessage failure) +++
                                   "\r\n" +++ failureBody failure }
              where
                swapFromTo m@(Message { messageTo = t, messageFrom = f }) =
                    m { messageTo = f, messageFrom = t }
      replicateM_ (knobReplies knobs) $ do
        delay $ floor $ 1000000 * knobDelay knobs
        generateMessage maybeKey
                        (Just (recvTime, recvAddr))
                        (hostAddr recvAddr)
                        replyMessage
      exitSuccess
    reapChildren
