module Main where

import Grumble.Prelude
import Grumble.Client

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName (setLevel DEBUG)

  let connParams = ConnectParams "irc.freenode.net" 6666
      usrCfg = UserConfig "caconym_test" "caconym_test"
      cltCfg = ClientConfig "caconym_test" usrCfg connParams

  debugM rootLoggerName ("Using client parameters: " ++ show cltCfg)

  client <- getClient cltCfg

  putStrLn "Press enter to quit"
  getLine

  cltQuit client
