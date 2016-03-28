module Main where

import Grumble.Prelude
import Grumble.Connection
import Grumble.Client
import Grumble.Client.Responders

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName (setLevel DEBUG)

  let connParams = ConnectParams "irc.freenode.net" 6666
      usrCfg = UserConfig "caconym_test" "caconym_test"
      cltCfg = ClientConfig "caconym_test" usrCfg connParams

  debugM rootLoggerName ("Using client parameters: " ++ show cltCfg)

  let resps = [ pingPong
              , nickRetry "caconym_test"
              , listenMotdEnd ]
  (sendMsg, msgs, cltAsync) <- runClient cltCfg resps

  async (msgListener msgs)
  putStrLn "Press enter to quit"
  getLine

  sendMsg $ Message Nothing QUIT (Parameters [] Nothing)
  wait cltAsync

msgListener :: Chan ClientMessage -> IO ()
msgListener incoming = forever $ do
  msg <- readChan incoming
  case msg of
    ClientUpdate u -> debugM rootLoggerName (show u)
    _ -> return ()
