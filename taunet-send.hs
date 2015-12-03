{-# LANGUAGE OverloadedStrings #-}
-- Copyright © 2015 Bart Massey
-- [This program is licensed under the GPL version 3 or later.]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- TauNet echo server

import qualified Data.ByteString.Char8 as BSC
import Network.BSD
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import System.Console.ParseArgs
import System.IO

import LocalAddr
import TaunetUtil

data ArgIndex = ArgPlain | ArgPing | ArgKeepCRLF | ArgDest
              deriving (Eq, Ord, Enum, Show)

argd :: [ Arg ArgIndex ]
argd = [ Arg {
           argIndex = ArgPlain,
           argAbbr = Just 'p',
           argName = Just "plain",
           argData = Nothing,
           argDesc = "Do not encrypt the incoming or outgoing messages." },
         Arg {
           argIndex = ArgPing,
           argAbbr = Just 'P',
           argName = Just "ping",
           argData = Nothing,
           argDesc = "Send a zero-length message; do not wait for reply." },
         Arg {
           argIndex = ArgKeepCRLF,
           argAbbr = Just 'c',
           argName = Just "cr",
           argData = Nothing,
           argDesc = "Do not do CRLF processing on input or output." },
         Arg {
           argIndex = ArgDest,
           argAbbr = Nothing,
           argName = Nothing,
           argData = argDataDefaulted "hostname" ArgtypeString "localhost",
           argDesc = "Destination host." } ]

main :: IO ()
main = do
  localAddresses <- getLocalAddresses
  argv <- parseArgsIO ArgsComplete argd
  let encrypted = not $ gotArg argv ArgPlain
  let echoing = gotArg argv ArgPing
  maybeKey <- maybeGetKey (encrypted && not echoing)
  let dest = getRequiredArg argv ArgDest
  hostEntry <- getHostByName dest
  let ha = AddressDataIPv4 $ hostAddress hostEntry
  let waitForIt = ha `notElem` localAddresses
  maybeListenSocket <-
      case waitForIt && not echoing of
        False ->
          return Nothing
        True -> do
          listenSocket <- socket AF_INET Stream defaultProtocol
          setSocketOption listenSocket ReuseAddr 1
          bind listenSocket $ portAddr taunetPort (AddressDataIPv4 0)
          listen listenSocket 1
          return $ Just listenSocket
  let keepCRLF = gotArg argv ArgKeepCRLF
  messagetext <- case echoing of
                   False -> do
                     msg <- BSC.hGetContents stdin
                     case keepCRLF of
                       True -> return msg
                       False -> return $ BSC.pack $ unlinesCRLF $
                                  lines $ BSC.unpack msg
                   True -> return ""
  sendMessage maybeKey (portAddr taunetPort ha) messagetext
  case maybeListenSocket of
    Nothing -> return ()
    Just listenSocket -> do
      (recvSocket, _) <- accept listenSocket
      reply <- receiveMessage Nothing maybeKey recvSocket
      close listenSocket
      putStr $ case keepCRLF of
                     True -> BSC.unpack reply
                     False -> unlines $ linesCRLF $ BSC.unpack reply
