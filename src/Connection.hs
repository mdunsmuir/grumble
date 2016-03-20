module Connection
( ConnectParams (..)
, Connection (..)
, getConnection
, module Message
) where

import Control.Concurrent.MVar
import System.Log.Logger
import Network.Socket hiding (send, recv)
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as BSock
import qualified Data.ByteString as B
import Grelude
import Message

data ConnectParams = ConnectParams
                   { cpHost :: String
                   , cpPortNumber :: PortNumber
                   } deriving Show

data Connection = Connection
                { sendMessage :: Message -> IO ()
                , incomingMessages :: Chan Message
                , close :: IO () }

getSocket :: IO Socket
getSocket = socket AF_INET Stream defaultProtocol

getConnection :: ConnectParams -> IO Connection
getConnection ConnectParams{..} = do
  -- fetch address information
  addrInfos <- getAddrInfo Nothing (Just cpHost) Nothing
  addrInfo <- case addrInfos of
    (ai:_) -> return ai
    _      -> throwIO CouldNotResolveHostName
  debugM rootLoggerName ("Found addrInfo: " ++ show addrInfo)
  let (SockAddrInet _ host) = addrAddress addrInfo
      addr = SockAddrInet cpPortNumber host
  debugM rootLoggerName ("Will use address: " ++ show addr)

  -- create and connect the socket
  sock <- getSocket
  connect sock addr
  mSock <- newMVar sock

  incoming <- newChan
  async $ recv sock incoming

  return $ Connection (withMVar mSock . send) incoming (withMVar mSock Sock.close)

send :: Message -> Socket -> IO ()
send msg sock = do
  debugM rootLoggerName ("Sending message: " ++ show msg)
  BSock.sendAll sock (encode msg)

recv :: Socket -> Chan Message -> IO ()
recv sock incoming = do
  raw <- BSock.recv sock 512

  when (B.null raw) $ do
    writeChan incoming ReadFailure
    throwIO ServerClosedConnection

  let msgs = decode raw
  forM_ msgs $ \msg -> do
    debugM rootLoggerName ("Received message: " ++ show msg)
    writeChan incoming msg
  recv sock incoming
