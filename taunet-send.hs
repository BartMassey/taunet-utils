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

import LocalAddr

maxMessageSize :: Int
maxMessageSize = 1024 * 1024

taunetPort :: PortNumber
taunetPort = 6283

(+++) :: BSC.ByteString -> BSC.ByteString -> BSC.ByteString
(+++) = BSC.append

sendMessage :: Socket -> IO ()
sendMessage s = do
  messagetext <- BSC.hGetContents stdin
  sendAll s messagetext
  close s

recvAll :: Socket -> IO BSC.ByteString
recvAll s = do
  bytes <- recv s maxMessageSize
  case BSC.length bytes of
    0 -> return bytes
    _ -> return . (bytes +++) =<< recvAll s

receiveMessage :: Socket -> IO BSC.ByteString
receiveMessage s = do
  messagetext <- recvAll s
  close s
  return messagetext

main :: IO ()
main = do
  localAddresses <- getLocalAddresses
  [ dest ] <- getArgs
  hostEntry <- getHostByName dest
  let ha = AddressDataIPv4 $ hostAddress hostEntry
  let waitForIt = ha `notElem` localAddresses
  maybeListenSocket <-
      case waitForIt of
        False ->
          return Nothing
        True -> do
          listenSocket <- socket AF_INET Stream defaultProtocol
          bind listenSocket $ portAddr taunetPort (AddressDataIPv4 0)
          listen listenSocket 1
          return $ Just listenSocket
  sendSocket <- socket AF_INET Stream defaultProtocol
  connect sendSocket $ portAddr taunetPort ha
  sendMessage sendSocket
  case maybeListenSocket of
    Nothing -> return ()
    Just listenSocket -> receiveMessage listenSocket >>= BSC.putStr

