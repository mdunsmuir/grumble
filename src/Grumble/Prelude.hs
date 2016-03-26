module Grumble.Prelude 
( GrumbleException (..)
, ErrorReplyCode (..)
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
                      | LostConnection
                      | ReplyError ErrorReplyCode
                        deriving (Show, Typeable)

instance Exception GrumbleException where

data ErrorReplyCode = ERR_DUMBHACK_FOR_ENUM_RANGE
                    | ERR_NOSUCHNICK
                    | ERR_NOSUCHSERVER
                    | ERR_NOSUCHCHANNEL
                    | ERR_CANNOTSENDTOCHAN
                    | ERR_TOOMANYCHANNELS
                    | ERR_WASNOSUCHNICK
                    | ERR_TOOMANYTARGETS
                    | ERR_NOORIGIN
                    | ERR_NORECIPIENT
                    | ERR_NOTEXTTOSEND
                    | ERR_NOTOPLEVEL
                    | ERR_WILDTOPLEVEL
                    | ERR_UNKNOWNCOMMAND
                    | ERR_NOMOTD
                    | ERR_NOADMININFO
                    | ERR_FILEERROR
                    | ERR_NONICKNAMEGIVEN
                    | ERR_ERRONEUSNICKNAME
                    | ERR_NICKNAMEINUSE
                    | ERR_NICKCOLLISION
                    | ERR_USERNOTINCHANNEL
                    | ERR_NOTONCHANNEL
                    | ERR_USERONCHANNEL
                    | ERR_NOLOGIN
                    | ERR_SUMMONDISABLED
                    | ERR_USERSDISABLED
                    | ERR_NOTREGISTERED
                    | ERR_NEEDMOREPARAMS
                    | ERR_ALREADYREGISTRED
                    | ERR_NOPERMFORHOST
                    | ERR_PASSWDMISMATCH
                    | ERR_YOUREBANNEDCREEP
                    | ERR_KEYSET
                    | ERR_CHANNELISFULL
                    | ERR_UNKNOWNMODE
                    | ERR_INVITEONLYCHAN
                    | ERR_BANNEDFROMCHAN
                    | ERR_BADCHANNELKEY
                    | ERR_NOPRIVILEGES
                    | ERR_CHANOPRIVSNEEDED
                    | ERR_CANTKILLSERVER
                    | ERR_NOOPERHOST
                    | ERR_UMODEUNKNOWNFLAG
                    | ERR_USERSDONTMATCH
                      deriving (Enum, Read, Show)
