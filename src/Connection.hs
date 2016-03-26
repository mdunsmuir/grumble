module Connection
( ConnectParams (..)
, Connection (..)
, getConnection
, module Message
) where

import Control.Concurrent.MVar
import System.Log.Logger
import Network.Socket hiding (send, recv, close)
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

-- | Get a new socket of the sort we like
getSocket :: IO Socket
getSocket = socket AF_INET Stream defaultProtocol

-- | Open a 'Connection' that will supply the other parts of the client
--   with the ability to communicate with the server
getConnection :: ConnectParams -> IO Connection
getConnection ConnectParams{..} = do
  sock <- getSocket'
  sockM <- newMVar sock
  
  recvChan <- do
    chan <- newChan

    let loop frags = do
          raw <- BSock.recv sock 512

          -- An empty read means the connection is no longer active
          when (B.null raw) $ do
            writeChan chan LostConnection
            throwIO ServerClosedConnection

          let (msgs, frags') = parseIncoming frags raw

          forM_ msgs $ \msg -> do
            debugM rootLoggerName ("Received message: " ++ show msg)
            writeChan chan msg

          loop frags'
      
    async (loop [])
    return chan

  return $ Connection (withMVar sockM . send) recvChan (withMVar sockM Sock.close)

  where
    getSocket' = do
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
      return sock

-- | Send a message to the server
send :: Message -> Socket -> IO ()
send msg sock = do
  debugM rootLoggerName ("Sending message: " ++ show msg)
  BSock.sendAll sock (encode msg)
