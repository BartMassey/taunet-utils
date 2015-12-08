-- Copyright © 2015 Bart Massey
-- [This program is licensed under the GPL version 3 or later.]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- | Miscellaneous utility functions used by `taunet-utils`.
-- These functions comprise shared functionality and
-- factorable stuff.
module TaunetUtil (
-- * 'ByteString' append operator.
  (+++),
-- * TauNet configuration constants.
  versionNumber, taunetPort, maxMessageSize,
-- * TauNet encryption stuff.
  scheduleReps, makeIV, readKey, maybeGetKey,
-- * CRLF support.
  linesCRLF, unlinesCRLF,
-- * Generic monadic actions.
  failUnless, repeat1M,
-- * Network send and receive.
  receiveMessage, sendMessage, lookupHost,
-- * Usermap
  UserData(..), UserMap, getUserMap, lookupUserMap, printUserMap )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.CipherSaber2
import Data.List (intercalate)
import qualified Data.Map as M
import Network.BSD
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.IO
import Text.Printf
import Text.SSV

import LocalAddr

-- | Append two bytestrings. Synonym for 'Data.ByteString.Append'.
(+++) :: BSC.ByteString -> BSC.ByteString -> BSC.ByteString
(+++) = BSC.append

-- | Current official TauNet version number, as a 'String'.
versionNumber :: String
versionNumber = "0.2"

-- | The official TauNet port is 6283, tau to four places.
taunetPort :: PortNumber
taunetPort = 6283

-- | Current TauNet protocol max message size.
maxMessageSize :: Int
maxMessageSize = 1024

-- | Number of CipherSaber-2 key scheduling repetitions in the
-- current TauNet protocol.
scheduleReps :: Int
scheduleReps = 20

-- | Use hardware random number generator to make a random
-- 10-byte IV. Currently Linux-dependent.
makeIV :: IO BS.ByteString
makeIV = 
  withBinaryFile "/dev/urandom" ReadMode $ \h ->
  do
    hSetBuffering h NoBuffering
    BS.hGet h 10

-- | Read a TauNet RC4 keystring from "key.txt". This
-- should clearly take the filename as an argument.
readKey :: IO BS.ByteString
readKey = do
  key <- readFile "key.txt"
  let keyLines = lines key
  case keyLines of
    [] -> error "failed to read any key lines"
    (keyStr : _) -> return $ BSC.pack keyStr

-- | Return an encryption key via 'readKey' if and only if
-- asked to. This is awkward at best.
maybeGetKey :: Bool -> IO (Maybe BS.ByteString)
maybeGetKey True = readKey >>= return . Just
maybeGetKey False = return Nothing

-- | When the condition is 'True', 'failUnless' does nothing. When
-- the condition is false, 'failUnless' returns the supplied
-- failure value.
failUnless :: Bool -> Either a () -> Either a ()
failUnless True _ = Right ()
failUnless False f = f

-- | Repeatedly execute a monadic action, at least once, discarding
-- all but the last result and returning it.
repeat1M :: Monad m => Int -> m a -> m a
repeat1M n _ | n <= 0 = error $ "repeat1M: insufficient count " ++ show n
repeat1M 1 a = a
repeat1M n a = a >> repeat1M (n - 1) a

-- | Like 'Data.List.lines', but for CRLF data.
linesCRLF :: String -> [String]
linesCRLF [] = []
linesCRLF ('\r' : '\n' : cs) =
    [] : linesCRLF cs
linesCRLF (c : cs) =
    case linesCRLF cs of
      [] -> [[c]]   -- String did not end in CRLF
      (l : ls) -> (c : l) : ls

-- | Like 'Data.List.unlines', but for CRLF data.
unlinesCRLF :: [String] -> String
unlinesCRLF = intercalate "\r\n"

-- | Up to some possible limit, retrieve all the
-- data available on the supplied 'Socket' and return
-- it as a 'ByteString'.
recvAll :: Maybe Int -> Socket -> IO BS.ByteString
recvAll maybeRemaining s
    | remainder = do
        -- XXX Pick a receive size here, really.
        bytes <- recv s 4096
        case BS.length bytes of
          0 -> return bytes
          n -> do
            rest <- recvAll (fmap (`subtract` n) maybeRemaining) s
            return $ bytes +++ rest
    | otherwise = return BS.empty
      where
        remainder =
            case maybeRemaining of
              Nothing -> True
              Just n -> n > 0

-- | Retrieve and possibly decrypt the data of a TauNet message.
receiveMessage :: Maybe Int -> Maybe BS.ByteString -> Socket
               -> IO BS.ByteString
receiveMessage maybeLimit maybeKey s = do
  messagetext <- recvAll maybeLimit s
  close s
  return $ maybeDecrypt messagetext
  where
    maybeDecrypt text =
        case maybeKey of
          Just key -> decrypt scheduleReps key text
          Nothing -> text

-- | Possibly encrypt and then send the data of a TauNet message.
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
  sendAll sendSocket messagetext
  close sendSocket
  return ()

lookupHost :: String -> IO (Maybe AddressData)
lookupHost dest = do
  hostEntry <- getHostByName dest
  -- XXX For now, assume lookup always succeeds.
  return $ Just $ AddressDataIPv4 $ hostAddress hostEntry

data UserData = UserData {
      userDataId :: String,
      userDataHost :: String,
      userDataFullname :: Maybe String }

type UserMap = M.Map String UserData

getUserMap :: IO UserMap
getUserMap = do
  csvFile <- readFile "node-table.csv"
  let records = map readRecord $ readCSV csvFile
  return $ M.fromList records
  where
    readRecord [userId, userHost, userFullname] =
        (userId, UserData userId userHost maybeFullname)
        where
          maybeFullname =
              let cleanName = unwords $ words userFullname in
              case cleanName of
                "" -> Nothing
                n -> Just n
    readRecord _ = error "bad user table"

lookupUserMap :: UserMap -> String -> Maybe UserData
lookupUserMap userMap userId = M.lookup userId userMap

printUserMap :: UserMap -> IO ()
printUserMap userMap = do
  flip mapM_ (M.elems userMap) $ (\u ->
      case (userDataFullname u) of
        Nothing ->
            printf "%s\t%s\n"
                   (userDataId u)
                   (userDataHost u)
        Just fn -> do
            printf "%s\t%s\t%s\n"
                   (userDataId u)
                   (userDataHost u)
                   fn )
