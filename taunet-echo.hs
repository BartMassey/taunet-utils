-- Copyright © 2015 Bart Massey
-- [This program is licensed under the GPL version 3 or later.]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

{-# LANGUAGE OverloadedStrings #-}
-- TauNet echo server

import Control.Monad
import Data.CipherSaber2
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.IO

taunetPort :: PortNumber
taunetPort = 6283

maxMessageSize :: Int
maxMessageSize = 1024 * 1024

scheduleReps :: Int
scheduleReps = 20

makeIV :: IO BS.ByteString
makeIV = 
  withBinaryFile "/dev/urandom" ReadMode $ \h ->
  do
    hSetBuffering h NoBuffering
    BS.hGet h 10

readKey :: IO BS.ByteString
readKey = do
  key <- readFile "key.txt"
  return $ BSC.pack $ head $ lines key

receiveMessage :: BS.ByteString -> Socket -> IO BS.ByteString
receiveMessage key s = do
  ciphertext <- recv s maxMessageSize
  return $ decrypt scheduleReps key ciphertext

sendMessage :: BS.ByteString -> Socket -> BS.ByteString -> IO ()
sendMessage key s body = do
  let plaintext =
          "version 0.2\r\n" `BS.append`
          "to: fixme\r\n" `BS.append`
          "from: po8\r\n\r\n" `BS.append`
          body
  iv <- makeIV
  let ciphertext = encrypt scheduleReps key iv plaintext
  _ <- send s ciphertext
  return ()

fixAddr :: SockAddr -> SockAddr
fixAddr (SockAddrInet _ address) = SockAddrInet taunetPort address
fixAddr _ = error "address type not supported"

main :: IO ()
main = do
  key <- readKey
  taunetSocket <- socket AF_INET Stream defaultProtocol
  bind taunetSocket $ SockAddrInet taunetPort 0
  listen taunetSocket 1
  forever $ do
    (recvSocket, recvAddr) <- accept taunetSocket
    receivedMessage <- receiveMessage key recvSocket
    close recvSocket
    sendSocket <- socket AF_INET Stream defaultProtocol
    connect sendSocket (fixAddr recvAddr)
    sendMessage key sendSocket receivedMessage
    close sendSocket
