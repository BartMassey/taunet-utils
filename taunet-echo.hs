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
import System.Console.ParseArgs
import System.Exit
import System.IO
import System.Posix.Process
import Network.Info

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
             deriving Show

data Failure = Failure { failureMessage :: String,
                         failureBody :: BS.ByteString }
             deriving Show

failUnless :: Bool -> Failure -> Either Failure ()
failUnless True _ = Right ()
failUnless False f = Left f

recvAll :: Socket -> IO BS.ByteString
recvAll s = do
  bytes <- recv s maxMessageSize
  case BS.length bytes of
    0 -> return bytes
    _ -> return . (bytes +++) =<< recvAll s

receiveMessage :: Bool -> BS.ByteString -> Socket
               -> IO (Either Failure Message)
receiveMessage doCrypt key s = do
  messagetext <- recvAll s
  close s
  let plaintext =
          case doCrypt of
            True -> decrypt scheduleReps key messagetext
            False -> messagetext
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
    toHeader <- removeHeader "to" $ headers !! 1
    fromHeader <- removeHeader "from" $ headers !! 2
    return $ Message toHeader fromHeader plaintext

sendMessage :: Bool -> BS.ByteString -> SockAddr -> Message -> IO ()
sendMessage doCrypt key sendAddr message = do
  let plaintext =
        ("version 0.2\r\n" +++
        "to: " +++ toPerson +++ "\r\n" +++
        "from: " +++ fromPerson +++ "\r\n\r\n" +++
        messageBody message) :: BSC.ByteString
        where
          toPerson = BSC.pack $ messageFrom message
          fromPerson = BSC.pack $ messageTo message
  messagetext <-
      case doCrypt of
        True -> do
          iv <- makeIV
          return $ encrypt scheduleReps key iv plaintext
        False ->
          return plaintext
  sendSocket <- socket AF_INET Stream defaultProtocol
  connect sendSocket sendAddr
  _ <- send sendSocket messagetext
  close sendSocket
  return ()

getLocalAddresses :: IO [HostAddress]
getLocalAddresses = do
  ifaces <- getNetworkInterfaces
  return $ map ((\(IPv4 a) -> a) . ipv4) ifaces

hostAddr :: SockAddr -> HostAddress
hostAddr (SockAddrInet _ ha) = ha
hostAddr _ = error "unsupported address type"

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
  let encrypted = not $ gotArg argv ArgPlain
  let debug = gotArg argv ArgDebug
  localAddresses <- getLocalAddresses
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
      when (ha `elem` localAddresses) exitSuccess
      let sendIt = sendMessage encrypted key $ SockAddrInet taunetPort ha
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
