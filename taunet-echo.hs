{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- Copyright © 2015 Bart Massey
-- [This program is licensed under the GPL version 3 or later.]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- TauNet echo server

import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import System.Console.ParseArgs
import System.Exit
import System.Posix.Process

import LocalAddr
import TaunetUtil

data ArgIndex = ArgPlain | ArgDebug
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
           argDesc = "Do debugging things." } ]

main :: IO ()
main = do
  argv <- parseArgsIO ArgsComplete argd
  let !encrypted = not $ gotArg argv ArgPlain
  let !debug = gotArg argv ArgDebug
  key <- readKey
  taunetSocket <- socket AF_INET Stream defaultProtocol
  bind taunetSocket $ SockAddrInet taunetPort 0
  listen taunetSocket 1
  forever $ do
    (recvSocket, recvAddr) <- accept taunetSocket
    _ <- forkProcess $ do
      received <- receiveMessage encrypted key recvSocket
      when debug $ print received
      let ha = hostAddr recvAddr
      localAddresses <- getLocalAddresses
      when (ha `elem` localAddresses) exitSuccess
      let sendIt = sendMessage encrypted key $ portAddr taunetPort ha
      case received of
        Right message -> sendIt message
        Left failure -> sendIt failMessage
                   where
                     failMessage = Message {
                       messageTo = "???",
                       messageFrom = "po8",
                       messageBody =
                           BSC.pack (failureMessage failure) +++
                           "\r\n" +++ failureBody failure }
      exitSuccess
    return ()
