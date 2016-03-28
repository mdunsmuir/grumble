module Grumble.Client.Monad 
( runClient
, getMessage
, sendMessage
) where

import Control.Monad.State
import Control.Monad.Reader
import Grumble.Prelude
import Grumble.Message
import Grumble.Connection
import Grumble.Client.Types

runClient :: ClientConfig
          -> [Responder]
          -> IO (Message -> IO (), Chan ClientMessage, Async ())

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
  return (conSendMessage, chan, as)

clientLoop :: IO () -> Chan ClientMessage -> ClientM ()
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
          (updates, rs') <- runReaderT rspAction msg
          liftIO $ forM_ updates (writeChan outgoingUpdates . ClientUpdate)
          return (rs ++ rs')

    responders' <- foldM respLoop [] =<< _responders <$> get
    modify $ \s -> s { _responders = responders' }

  where
    finalize = do
      liftIO $ debugM rootLoggerName "Lost connection; running finalizers"
      resps <- _responders <$> get
      runReaderT (sequence (map rspFinalize resps)) NoMoreMessages
      liftIO masterFinalizer

getMessage :: ResponderM Message
getMessage = ask

sendMessage :: Message -> ResponderM ()
sendMessage msg = do
  sender <- _sendMessage <$> get
  liftIO $ sender msg
