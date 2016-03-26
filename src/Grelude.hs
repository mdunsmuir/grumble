module Grelude 
( Message (..)
, Prefix
, Command (..)
, Parameters (..)
, Parameter
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
import qualified Data.ByteString as B
import Data.Typeable

data Message = Message
             { msgPrefix :: Maybe Prefix
             , msgCommand :: Command
             , msgParams :: Parameters }
             
             | UnDecodable B.ByteString String
             | LostConnection
               deriving Show

type Prefix = String

data Command = JOIN
             | MODE
             | NICK
             | NOTICE
             | PING
             | PONG
             | PRIVMSG 
             | USER
             | Numeric Int
               deriving (Read, Show)

data Parameters = Parameters
                { parRegulars :: [Parameter]
                , parTrailing :: Maybe Parameter
                } deriving Show

type Parameter = String

data GrumbleException = CouldNotResolveHostName
                      | ServerClosedConnection
                      | UnEncodableMessage Message
                      | UnDecodableMessage String
                        deriving (Show, Typeable)

instance Exception GrumbleException where
