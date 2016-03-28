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

  (sendMsg, _, cltAsync) <- runClient cltCfg [pingPong]

  putStrLn "Press enter to quit"
  getLine

  sendMsg $ Message Nothing QUIT (Parameters [] Nothing)
  wait cltAsync
