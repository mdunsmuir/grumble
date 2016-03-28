module Grumble.Client.Types
( UserConfig (..)
, ClientConfig (..)
, ClientState (..)
, ClientM
, Responder (..)
, ResponderAction
, ResponderM
, Update (..)
, ClientMessage (..)
) where

import Control.Monad.State
import Control.Monad.Reader
import Grumble.Prelude
import Grumble.Message
import Grumble.Connection

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

data Update = PingPong
              deriving Show

data ClientMessage = ClientUpdate Update
                   | ClientIncomingMessage Message
