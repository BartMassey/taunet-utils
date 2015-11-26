{-# LANGUAGE OverloadedStrings #-}
-- Copyright © 2015 Bart Massey
-- [This program is licensed under the GPL version 3 or later.]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- TauNet echo server

import Control.Monad
import Data.CipherSaber2
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.Exit
import System.IO
import System.Posix.Process

(+++) :: BSC.ByteString -> BSC.ByteString -> BSC.ByteString
(+++) = BSC.append

versionNumber :: String
versionNumber = "0.2"

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

linesCRLF :: String -> [String]
linesCRLF [] = []
linesCRLF ('\r' : '\n' : cs) =
    [] : linesCRLF cs
linesCRLF (c : cs) =
    case linesCRLF cs of
      [] -> [[c]]   -- String did not end in CRLF
      (l : ls) -> (c : l) : ls

data Message = Message { messageTo, messageFrom :: String,
                         messageBody :: BS.ByteString }

data Failure = Failure { failureMessage :: String,
                         failureBody :: BS.ByteString }

failUnless :: Bool -> Failure -> Either Failure ()
failUnless True _ = Right ()
failUnless False f = Left f

receiveMessage :: BS.ByteString -> Socket -> IO (Either Failure Message)
receiveMessage key s = do
  ciphertext <- recv s maxMessageSize
  close s
  let plaintext = decrypt scheduleReps key ciphertext
  let headers = take 4 $ linesCRLF $ BSC.unpack plaintext
  return $ do
    failUnless (length headers == 4) $
               Failure "mangled headers" plaintext
    failUnless (headers !! 0 == ("version " ++ versionNumber)) $
               Failure "bad version header" plaintext
    failUnless (headers !! 3 == "") $
               Failure "bad end-of-headers" plaintext
    toHeader <- removeHeader "to" (headers !! 1) plaintext
    fromHeader <- removeHeader "from" (headers !! 2) plaintext
    Right $ Message toHeader fromHeader plaintext
  where
    removeHeader header target plaintext
        | isPrefixOf paddedHeader target =
            Right $ drop (length paddedHeader) target
        | otherwise =
            Left $ Failure ("bad header " ++ header) plaintext
        where
          paddedHeader = header ++ ": "

sendMessage :: BS.ByteString -> SockAddr -> Message -> IO ()
sendMessage key sendAddr message = do
  let plaintext =
        ("version 0.2\r\n" +++
        "to: " +++ toPerson +++ "\r\n" +++
        "from: " +++ fromPerson +++ "\r\n\r\n" +++
        messageBody message) :: BSC.ByteString
        where
          toPerson = BSC.pack $ messageFrom message
          fromPerson = BSC.pack $ messageTo message
  iv <- makeIV
  let ciphertext = encrypt scheduleReps key iv plaintext
  sendSocket <- socket AF_INET Stream defaultProtocol
  connect sendSocket sendAddr
  _ <- send sendSocket ciphertext
  close sendSocket
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
    _ <- forkProcess $ do
      let sendAddr = fixAddr recvAddr
      received <- receiveMessage key recvSocket
      case received of
        Right message -> sendMessage key sendAddr message
        Left failure -> sendMessage key sendAddr failMessage
                   where
                     failMessage = Message {
                       messageTo = "???",
                       messageFrom = "po8",
                       messageBody =
                           BSC.pack (failureMessage failure) +++
                           "\r\n" +++ failureBody failure }
      exitSuccess
    return ()
