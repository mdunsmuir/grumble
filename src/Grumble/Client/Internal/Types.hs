module Grumble.Client.Internal.Types
( Client (..)
, ClientMessage (..)

, UserConfig (..)
, ClientConfig (..)

, ResponderM (..)
, Responder (..)
, ResponderEmission (..)

, ClientM
, ClientState (..)
) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
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

data ClientState u = ClientState
                   { _info :: ()
                   , _sendMessage :: Message -> IO ()
                   , _incomingMessages :: Chan Message
                   , _responders :: [Responder u] } 

type ClientM u = StateT (ClientState u) IO

-- Responder stuff

data ResponderEmission u = EmitResponder (Responder u)
                         | EmitUpdate u

-- | Responders run in this monad.
newtype ResponderM u a = ResponderM (WriterT [ResponderEmission u] (ReaderT Message (ClientM u)) a)
                         deriving (Functor, Applicative, Monad)

instance MonadIO (ResponderM u) where
  liftIO = ResponderM . liftIO

data Responder u = Responder
                 { rspAction :: ResponderM u ()
                 , rspFinalize :: ResponderM u () }

data ClientMessage u = ClientUpdate u
                     | ClientIncomingMessage Message

data Client u = Client
              { cltSendMessage :: Message -> IO () -- ^ Use this to send messages to the IRC server.
              , cltMessages :: Chan (ClientMessage u) -- ^ Updates from your responders and all incoming 
                                                      --   messages appear in this channel.
              , cltAsync :: Async () -- ^ An 'Async' handle for the client's thread.
              }
