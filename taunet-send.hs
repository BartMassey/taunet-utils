{-# LANGUAGE OverloadedStrings #-}
-- Copyright © 2015 Bart Massey
-- [This program is licensed under the GPL version 3 or later.]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- TauNet echo server

import qualified Data.ByteString.Char8 as BSC
import Network.BSD
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import System.Environment
import System.IO

import LocalAddr
import TaunetUtil

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
  messagetext <- BSC.hGetContents stdin
  sendMessage Nothing (portAddr taunetPort ha) messagetext
  case maybeListenSocket of
    Nothing -> return ()
    Just listenSocket -> do
      (recvSocket, _) <- accept listenSocket
      reply <- receiveMessage Nothing recvSocket
      close listenSocket
      BSC.putStr reply
