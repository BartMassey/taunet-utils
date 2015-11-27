{-# LANGUAGE OverloadedStrings #-}
-- Copyright © 2015 Bart Massey
-- [This program is licensed under the GPL version 3 or later.]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- TauNet echo server

import qualified Data.ByteString.Char8 as BSC
import Network.BSD
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.Environment
import System.IO

taunetPort :: PortNumber
taunetPort = 6283

sendMessage :: Socket -> IO ()
sendMessage s = do
  messagetext <- BSC.hGetContents stdin
  sendAll s messagetext
  close s

main :: IO ()
main = do
  [ dest ] <- getArgs
  hostEntry <- getHostByName dest
  taunetSocket <- socket AF_INET Stream defaultProtocol
  connect taunetSocket $ SockAddrInet taunetPort (hostAddress $ hostEntry)
  sendMessage taunetSocket
