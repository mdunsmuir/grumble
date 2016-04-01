module Grumble.Client
( -- * Client
  --
  -- | A 'Client' is a higher-level interface to an IRC server than a
  --   'Connection'.
  Client (..)
, ClientMessage (..)
, runClient

  -- * Configuration
, UserConfig (..)
, ClientConfig (..)

  -- * Responders
  --
  -- | Responders let clients of this library define behavior for handling
  --   incoming requests.
, ResponderM
, Responder (..)
, getMessage, sendMessage
, emitUpdate, emitResponder
) where

import Grumble.Client.Internal.Types
import Grumble.Client.Internal.Monad
