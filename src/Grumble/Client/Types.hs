-- | Throughout this module, the type parameter 'u' refers to the type of the
--   updates that responders can emit. See 'Client'.
module Grumble.Client.Types
( -- * Basic Client-Related Types
  Client (..)
, ClientMessage (..)

  -- * Configuration
, UserConfig (..)
, ClientConfig (..)

  -- * Responders
  --
  -- | Responders let clients of this library define behavior for handling
  --   incoming requests. See "Grumble.Client.Monad" for more.
, ResponderM
, Responder (..)
, ResponderEmission (..)

  -- * ClientM
  -- 
  -- | The main client loop runs in a state monad with state 'ClientState'.
  --   This is really only for use internal to this library.
, ClientM
, ClientState (..)

, Update (..)
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

-- | Responders run in this monad. See "Grumble.Client.Monad" for the
--   provided operations.
type ResponderM u = WriterT [ResponderEmission u] (ReaderT Message (ClientM u))

data Responder u = Responder
                 { rspAction :: ResponderM u () -- ^ foobar
                 , rspFinalize :: ResponderM u () }

-- Update message stuff (emitted to clients of Client)

data Update = PingPong
            | NickRetry String
            | NickAccepted String
            | EndMotd
              deriving Show

data ClientMessage u = ClientUpdate u
                     | ClientIncomingMessage Message

data Client u = Client
              { cltSendMessage :: Message -> IO () -- ^ Use this to send messages to the IRC server.
              , cltMessages :: Chan (ClientMessage u) -- ^ Updates from your responders and all incoming 
                                                      --   messages appear in this channel.
              , cltAsync :: Async () -- ^ An 'Async' handle for the client's thread.
              }
