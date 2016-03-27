module Grumble.Client.Responders
( pingPong
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
          Message _ PING params -> sendMessage (Message Nothing PONG params)
          _ -> return ()
        return ([PingPong], [pingPong]) 
  in  Responder action (return ())
