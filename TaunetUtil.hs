{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- Copyright © 2015 Bart Massey
-- [This program is licensed under the GPL version 3 or later.]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- taunet-utils utils

module TaunetUtil (
  (+++), versionNumber, taunetPort, maxMessageSize, scheduleReps,
  makeIV, readKey, linesCRLF,
  failUnless, receiveMessage, sendMessage )
where

import Data.CipherSaber2
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.IO

(+++) :: BSC.ByteString -> BSC.ByteString -> BSC.ByteString
(+++) = BSC.append

versionNumber :: String
versionNumber = "0.2"

taunetPort :: PortNumber
taunetPort = 6283

maxMessageSize :: Int
maxMessageSize = 10240

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

failUnless :: Bool -> a -> Either a ()
failUnless True _ = Right ()
failUnless False f = Left f

linesCRLF :: String -> [String]
linesCRLF [] = []
linesCRLF ('\r' : '\n' : cs) =
    [] : linesCRLF cs
linesCRLF (c : cs) =
    case linesCRLF cs of
      [] -> [[c]]   -- String did not end in CRLF
      (l : ls) -> (c : l) : ls

recvAll :: Socket -> IO BS.ByteString
recvAll s = do
  bytes <- recv s maxMessageSize
  case BS.length bytes of
    0 -> return bytes
    _ -> return . (bytes +++) =<< recvAll s

receiveMessage :: Maybe BS.ByteString -> Socket
               -> IO BS.ByteString
receiveMessage maybeKey s = do
  messagetext <- recvAll s
  close s
  let plaintext =
        case maybeKey of
          Just key -> decrypt scheduleReps key messagetext
          Nothing -> messagetext
  return plaintext

sendMessage :: Maybe BS.ByteString -> SockAddr -> BS.ByteString -> IO ()
sendMessage maybeKey sendAddr plaintext = do
  messagetext <-
      case maybeKey of
        Just key -> do
          iv <- makeIV
          return $ encrypt scheduleReps key iv plaintext
        Nothing ->
          return plaintext
  sendSocket <- socket AF_INET Stream defaultProtocol
  connect sendSocket sendAddr
  _ <- send sendSocket messagetext
  close sendSocket
  return ()
