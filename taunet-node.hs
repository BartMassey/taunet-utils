{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- Copyright © 2015 Bart Massey
-- [This program is licensed under the GPL version 3 or later.]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- TauNet node

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.IORef
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import System.Console.ParseArgs
import System.IO
import Text.Printf

import DateTime
import LocalAddr
import TaunetUtil
import TaunetMessage

debugMVar :: Bool
debugMVar = False

debugReceive :: Bool
debugReceive = False

data ArgIndex = ArgUser
              deriving (Eq, Ord, Enum, Show)

argd :: [ Arg ArgIndex ]
argd = [ Arg {
           argIndex = ArgUser,
           argDesc = "Specify TauNet username.",
           argAbbr = Nothing,
           argName = Nothing,
           argData = argDataRequired "username" ArgtypeString } ]

data Request = Display AddressData String (Either Failure Message)
             | Hold Bool

receiveThread :: MVar Request -> Maybe BS.ByteString -> IO ()
receiveThread requestBox maybeKey = do
  taunetSocket <- socket AF_INET Stream defaultProtocol
  setSocketOption taunetSocket ReuseAddr 1
  bind taunetSocket $ SockAddrInet taunetPort 0
  listen taunetSocket 1
  forever $ do
    when debugReceive $ putStrLn "*receiveThread: accept"
    (recvSocket, recvAddr) <- accept taunetSocket
    let ha = hostAddr recvAddr
    recvTime <- getTimeRFC3339
    when debugReceive $ putStrLn "*receiveThread: receiveMessage"
    plaintext <- receiveMessage
                   (Just (2 * maxMessageSize))
                   maybeKey
                   recvSocket
    when debugReceive $ BSC.putStrLn $
                        "*receiveThread: plaintext:\n" +++ plaintext
    when (BS.length plaintext > 0) $ do
      let received = parseMessage False plaintext
      when debugMVar $ putStrLn "*receiveThread: putMVar"
      putMVar requestBox (Display ha recvTime received)
      when debugMVar $ putStrLn "*receiveThread: put"

sendThread :: MVar Request -> Maybe BS.ByteString -> String
           -> IO ()
sendThread  requestBox maybeKey user = forever $ do
  _ <- getLine
  when debugMVar $ putStrLn "*sendThread: holding"
  putMVar requestBox (Hold True)
  when debugMVar $ putStrLn "*sendThread: held"
  putStr "to: "
  hFlush stdout
  toUser <- getLine
  putStrLn "Message: end with \".\" line to send or \"!\" line to abort"
  body <- readBody
  sendIt toUser body
  when debugMVar $ putStrLn "*sendThread: unholding"
  putMVar requestBox (Hold False)
  when debugMVar $ putStrLn "*sendThread: unheld"
  where
    readBody = do
      line <- getLine
      case line of
        "." -> return (Just [])
        "!" -> do
          putStrLn "Message aborted"
          return Nothing
        text -> do
          rest <- readBody
          return $ fmap (text :) rest
    sendIt _ Nothing = return ()
    sendIt dest (Just body) = do
      -- XXX kludge until address book goes in
      let destWords = words dest
      case length destWords of
        2 -> do
          let [toUser, toHost] = destWords
          maybeToAddr <- lookupHost toHost
          case maybeToAddr of
            Nothing -> putStrLn "host lookup failed"
            Just toAddr -> do
                let message = Message {
                    messageTo = toUser,
                    messageFrom = user,
                    messageCommands = [],
                    messageBody = BSC.pack $ unlines body }
                generateMessage maybeKey Nothing toAddr message
        _ -> putStrLn "could not parse destination"

displayThread :: MVar Request -> IO ()
displayThread requestBox = do
  held <- newIORef False
  holdQueue <- newIORef []
  forever $ do
    when debugMVar $ putStrLn "*displayThread: takeMVar"
    request <- takeMVar requestBox
    when debugMVar $ putStrLn "*displayThread: taken"
    case request of
      Hold b -> do
        writeIORef held b
      Display _ _ _ -> do
        qs <- readIORef holdQueue
        writeIORef holdQueue (request : qs)
    h <- readIORef held
    unless h $ do
      qs <- readIORef holdQueue
      mapM_ handler $ reverse qs
      where
        handler (Display ha recvTime (Left failure)) =
          printf "%s: failed message from %s: %s\n\n"
                 recvTime
                 (show ha)
                 (failureMessage failure)
        handler (Display ha recvTime (Right message)) = do
          printf "%s: message from %s\n"
                 recvTime
                 (show ha)
          putStrLn $ showMessage message
          hFlush stdout
        handler _ = error "internal error: bogus message in hold queue"

main :: IO ()
main = do
  argv <- parseArgsIO ArgsComplete argd
  let !user = getRequiredArg argv ArgUser
  maybeKey <- maybeGetKey True
  requestBox <- newEmptyMVar
  _ <- forkIO $ displayThread requestBox
  _ <- forkIO $ receiveThread requestBox maybeKey
  sendThread requestBox maybeKey user
