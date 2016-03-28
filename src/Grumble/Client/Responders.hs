module Grumble.Client.Responders
( pingPong
, nickRetry
, listenMotdEnd
) where

import Grumble.Prelude
import Grumble.Message
import Grumble.Client.Types
import Grumble.Client.Monad

pingPong :: Responder
pingPong =
  let action = do
        msg <- getMessage
        case msg of
          -- if PING, send PONG
          Message _ PING params -> do
            sendMessage (Message Nothing PONG params)
            return ([PingPong], [pingPong]) 
          _ -> return ([], [pingPong])
  in  Responder action (return ())

nickRetry :: String -> Responder
nickRetry lastAttempt = Responder action (return ())
  where
    -- we do the usual thing where we add underscores until the nick
    -- is accepted by the server
    thisAttempt = lastAttempt ++ "_"
    changeMsg = Message Nothing NICK (Parameters [thisAttempt] Nothing)

    action = do
      msg <- getMessage
      case msg of
        -- on nick taken, try changing it and re-register responder
        Message _ (Reply 433) _ -> do
          sendMessage changeMsg
          return ([NickRetry thisAttempt], [nickRetry thisAttempt]) 

        -- if our last attempt "took", we're done
        Message _ _ (Parameters (nick:_) _) ->
          return $ if nick == lastAttempt
            then ([NickAccepted lastAttempt], [])
            else ([], [nickRetry lastAttempt])
            

        -- otherwise keep listening
        _ -> return ([], [nickRetry lastAttempt])

listenMotdEnd :: Responder
listenMotdEnd = Responder action (return ())
  where
    action = do
      msg <- getMessage
      case msg of
        Message _ (Reply 376) _ -> return ([EndMotd], [])
        _ -> return ([], [listenMotdEnd])

