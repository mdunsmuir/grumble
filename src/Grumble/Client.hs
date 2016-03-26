module Grumble.Client
( Client (..)
, UserConfig (..)
, ClientConfig (..)
, ConnectParams (..)
, getClient
) where

import Grumble.Prelude
import Grumble.Message
import Grumble.Connection

data Client = Client
            { cltSendMessage :: Message -> IO ()
            , cltIncomingMessages :: Chan Message
            , cltQuit :: IO () }

data UserConfig = UserConfig
                { usrCfgUserName :: String
                , usrCfgRealName :: String
                } deriving Show

data ClientConfig = ClientConfig
                  { cltCfgNick :: String
                  , cltCfgUserConfig :: UserConfig
                  , cltCfgConnectParams :: ConnectParams
                  } deriving Show

getClient :: ClientConfig -> IO Client
getClient cc@ClientConfig{..} = do
  conn <- getConnection cltCfgConnectParams
  initialRegistration cc conn
  
  let quit = do
        sendMessage conn (Message Nothing QUIT (Parameters [] Nothing))
        close conn
      client = Client (sendMessage conn) (incomingMessages conn) quit

  pingPonger client
  return client
  
initialRegistration :: ClientConfig -> Connection -> IO ()
initialRegistration ClientConfig{..} Connection{..} = do
  let UserConfig{..} = cltCfgUserConfig
  sendMessage $ user usrCfgUserName usrCfgRealName
  sendMessage $ Message Nothing NICK (Parameters [cltCfgNick] Nothing)

pingPonger :: Client -> IO ()
pingPonger Client{..} = do
  inc <- dupChan cltIncomingMessages
  let loop = forever $ do
        msg <- readChan inc
        case msg of
          NoMoreMessages -> do debugM rootLoggerName "lost connection, shutting down ping-ponger"
                               throwIO LostConnection
          Message _ PING params -> cltSendMessage (Message Nothing PONG params)
          _ -> return ()
  async $ forever loop
  return ()
      
    
