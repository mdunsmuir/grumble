module Grumble.Client.Monad 
( runClient
, getMessage
, sendMessage
, emitUpdate
, emitResponder
) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Grumble.Prelude
import Grumble.Message
import Grumble.Connection
import Grumble.Client.Types

runClient :: ClientConfig -- ^ Configuration for the connection, nickname, and so on.
          -> [Responder u] -- ^ A list of initial responders to register.
          -> IO (Client u) -- ^ A 'Client' "handle".

runClient ClientConfig{..} resps = do
  Connection{..} <- getConnection cltCfgConnectParams
  let state = ClientState () conSendMessage conIncomingMessages resps
  chan <- newChan

  let initialRegistration =
        let UserConfig{..} = cltCfgUserConfig
            msgs = [ user usrCfgUserName usrCfgRealName
                   , Message Nothing NICK (Parameters [cltCfgNick] Nothing) ]
        in  forM_ msgs conSendMessage
  initialRegistration

  as <- async $ evalStateT (clientLoop conClose chan) state
  return $ Client conSendMessage chan as

clientLoop :: IO () -> Chan (ClientMessage u) -> ClientM u ()
clientLoop masterFinalizer outgoingUpdates =
  bracket_ (return ()) finalize $ forever $ do
    incomingMsgs <- _incomingMessages <$> get
    msg <- liftIO (readChan incomingMsgs)

    case msg of
      NoMoreMessages -> throwIO LostConnection
      _ -> return ()

    -- fold over responders, accumulate updated responders,
    -- and send out the messages
    
    let respLoop rs Responder{..}  = do
          (_, (updates, rs')) <- runResponderM msg rspAction
          liftIO $ forM_ updates (writeChan outgoingUpdates . ClientUpdate)
          return (rs ++ rs')

    responders' <- foldM respLoop [] =<< _responders <$> get
    modify $ \s -> s { _responders = responders' }

  where
    finalize = do
      liftIO $ debugM rootLoggerName "Lost connection; running finalizers"
      resps <- _responders <$> get
      runResponderM NoMoreMessages (sequence (map rspFinalize resps))
      liftIO masterFinalizer

runResponderM :: Message -> ResponderM u a -> ClientM u (a, ([u], [Responder u]))
runResponderM msg rspAction = do
  (ans, emitted) <- runReaderT (runWriterT rspAction) msg
  
  let f e (us, rs) = case e of
                       EmitResponder r -> (us, r:rs)
                       EmitUpdate u -> (u:us, rs)
  
  return $ (ans,) $ foldr f ([], []) emitted

-- | Get the message we're responding to.
getMessage :: ResponderM u Message
getMessage = ask

-- | Send a message to the IRC server.
sendMessage :: Message -> ResponderM u ()
sendMessage msg = do
  sender <- _sendMessage <$> get
  liftIO $ sender msg

-- | Emit an "update" i.e. 'u' throughout this module.
emitUpdate :: u -> ResponderM u ()
emitUpdate = tell . (:[]) . EmitUpdate

-- | Emit a 'Responder' to be added to the list of active responders.
emitResponder :: Responder u -> ResponderM u ()
emitResponder = tell . (:[]) . EmitResponder
