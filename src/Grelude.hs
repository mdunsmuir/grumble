module Grelude 
( Message (..)
, IncomingMessage (..)
, GrumbleException (..)
, module Control.Exception
, module Control.Monad
, module Control.Concurrent.Chan
, module Control.Concurrent.Async
) where

import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent.Async
import Control.Exception hiding (try)
import Text.Parsec (ParseError)
import Data.Typeable

data Message = NICK String
             | USER String String String String
             | PING String
             | PONG String
             | Unknown String
             | ReadFailure
               deriving Show

data IncomingMessage = IncomingWithPrefix String Message
                     | IncomingWithoutPrefix Message
                       deriving Show

data GrumbleException = CouldNotResolveHostName
                      | ServerClosedConnection
                      | UnEncodableMessage Message
                      | UnDecodableMessage String
                        deriving (Show, Typeable)

instance Exception GrumbleException where
