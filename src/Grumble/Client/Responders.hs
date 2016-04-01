module Grumble.Client.Responders
( Update (..)
, pingPong
, nickRetry
, listenMotdEnd
) where

import Grumble.Prelude
import Grumble.Message
import Grumble.Client


data Update = PingPong
            | NickRetry String
            | NickAccepted String
            | EndMotd
              deriving Show

noFinalizer :: ResponderM u () -> Responder u
noFinalizer act = Responder act (return ())

-- | Respond to pings with pongs. Forever.
pingPong :: Responder Update
pingPong = noFinalizer $ do
  msg <- getMessage
  case msg of
    -- if PING, send PONG
    Message _ PING params -> do
      sendMessage (Message Nothing PONG params)
      emitUpdate PingPong
    _ -> return ()
  emitResponder pingPong

-- | Retry nickname registration until we get one that isn't taken. We use the
--   normal IRC convention of appending underscores.
nickRetry :: String -> Responder Update
nickRetry lastAttempt = noFinalizer $ do
  let thisAttempt = lastAttempt ++ "_"
      changeMsg = Message Nothing NICK (Parameters [thisAttempt] Nothing)

  msg <- getMessage
  case msg of
    Message _ (Reply 433) _ -> do
      sendMessage changeMsg
      emitUpdate $ NickRetry thisAttempt
      emitResponder $ nickRetry thisAttempt

    Message _ _ (Parameters (nick:_) _) ->
      if nick == lastAttempt
        then emitUpdate $ NickAccepted lastAttempt
        else emitResponder $ nickRetry lastAttempt

    _ -> emitResponder $ nickRetry lastAttempt

listenMotdEnd :: Responder Update
listenMotdEnd = noFinalizer $ do
  msg <- getMessage
  case msg of
    Message _ (Reply 376) _ -> emitUpdate EndMotd
    _ -> emitResponder listenMotdEnd
