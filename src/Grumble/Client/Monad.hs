module Grumble.Client.Monad where

import Control.Monad.State
import Control.Monad.Reader
import Grumble.Prelude
import Grumble.Message
import Grumble.Client.Types

runClient :: Client -> [Responder] -> IO (Chan ClientMessage)
runClient Client{..} resps = do
  let state = ClientState () cltSendMessage cltIncomingMessages resps
  chan <- newChan
  async $ runStateT (clientLoop chan) state
  return chan

clientLoop :: Chan ClientMessage -> ClientM ()
clientLoop chan = forever $ do
  incomingMsgs <- _incomingMessages <$> get
  msg <- liftIO (readChan incomingMsgs)

  -- fold over responders, accumulate updated responders,
  -- and send out the messages
  undefined

runResponder :: Message -> ResponderAction
runResponder = undefined
