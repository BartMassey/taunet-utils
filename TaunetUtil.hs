{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- Copyright © 2015 Bart Massey
-- [This program is licensed under the GPL version 3 or later.]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- taunet-utils utils

module TaunetUtil (
  (+++), versionNumber, taunetPort, maxMessageSize, scheduleReps,
  makeIV, readKey, maybeGetKey, linesCRLF,
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
maxMessageSize = 1024

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
  let keyLines = lines key
  case keyLines of
    [] -> error "failed to read any key lines"
    (keyStr : _) -> return $ BSC.pack keyStr

maybeGetKey :: Bool -> IO (Maybe BS.ByteString)
maybeGetKey True = readKey >>= return . Just
maybeGetKey False = return Nothing

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

-- Returns 'Nothing' on oversized message.
recvAll :: Int -> Socket -> IO BS.ByteString
recvAll received s
    | received > maxMessageSize = return ""
    | otherwise = do
        bytes <- recv s (maxMessageSize - received)
        case BS.length bytes of
          0 -> return bytes
          n -> do
            rest <- recvAll (received + n) s
            return $ bytes +++ rest

-- Returns 'Nothing' on oversized message.
receiveMessage :: Maybe BS.ByteString -> Socket -> IO BS.ByteString
receiveMessage maybeKey s = do
  messagetext <- recvAll 0 s
  close s
  return $ maybeDecrypt messagetext
  where
    maybeDecrypt text =
        case maybeKey of
          Just key -> decrypt scheduleReps key text
          Nothing -> text

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
