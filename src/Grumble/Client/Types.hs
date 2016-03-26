module Grumble.Client.Types
( Client (..)
, UserConfig (..)
, ClientConfig (..)
, ClientState (..)
, ClientM
, Responder (..)
, ResponderAction
, ResponderM
, Update
, ClientMessage
) where

import Control.Monad.State
import Control.Monad.Reader
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

data ClientState = ClientState
                 { _info :: ()
                 , _sendMessage :: Message -> IO ()
                 , _incomingMessages :: Chan Message
                 , _responders :: [Responder] } 

type ClientM = StateT ClientState IO

-- Responder stuff

type ResponderM = ReaderT Message ClientM

type ResponderAction = ResponderM ([Update], [Responder])

data Responder = Responder
               { rspAction :: ResponderAction
               , rspFinalize :: ResponderM () }

-- Update message stuff (emitted to clients of Client)

data Update = Update

data ClientMessage = ClientUpdate Update
                   | ClientIncomingMessage Message
