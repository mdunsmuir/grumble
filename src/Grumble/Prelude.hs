module Grumble.Prelude 
( GrumbleException (..)
, module System.Log.Logger
, module Control.Exception
, module Control.Monad
, module Control.Concurrent.Chan
, module Control.Concurrent.Async
, module Data.Maybe
, module Data.Either
, module Data.String.Conversions
) where

import System.Log.Logger
import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent.Async
import Control.Exception hiding (try)
import Data.Maybe
import Data.Either
import Data.String.Conversions
import Data.Typeable

data GrumbleException = CouldNotResolveHostName
                      | ServerClosedConnection
                        deriving (Show, Typeable)

instance Exception GrumbleException where
