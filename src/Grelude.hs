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

type Prefix = String


data Command = ADMIN
             | AWAY
             | CNOTICE
             | CPRIVMSG
             | CONNECT
             | DIE
             | ENCAP
             | ERROR
             | HELP
             | INFORMATION
             | INVITE
             | ISON
             | JOIN
             | KICK
             | KILL
             | KNOCK
             | LINKS
             | LIST
             | LUSERS
             | MODE
             | MOTD
             | NAMES
             | NAMESX
             | NICK
             | NOTICE
             | OPER
             | PART
             | PASS
             | PING
             | PONG
             | PRIVMSG
             | QUIT
             | REHASH
             | RESTART
             | RULES
             | SERVER
             | SERVICE
             | SERVLIST
             | SQUERY
             | SQUIT
             | SETNAME
             | SILENCE
             | STATS
             | SUMMON
             | TIME
             | TOPIC
             | TRACE
             | UHNAMES
             | USER
             | USERHOST
             | USERIP
             | USERS
             | VERSION
             | WALLOPS
             | WATCH
             | WHO
             | WHOIS
             | WHOWAS
             | Numeric Int
               deriving (Read, Show)

data Parameters = Parameters
                { parRegulars :: [Parameter]
                , parTrailing :: Maybe Parameter
                } deriving Show

type Parameter = String

data GrumbleException = CouldNotResolveHostName
                      | ServerClosedConnection
                        deriving (Show, Typeable)

instance Exception GrumbleException where
