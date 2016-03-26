module Main where

import Control.Concurrent.Async
import System.Log.Logger
import Grelude
import Connection

main :: IO ()
main = do
  -- set up logger
  logger <- getRootLogger
  updateGlobalLogger rootLoggerName (setLevel DEBUG)

  -- connect
  conn <- getConnection (ConnectParams "irc.freenode.net" 6666)
  listenAsync <- async (listen conn)

  sendMessage conn (user "caconym_test" "caconym_test")
  sendMessage conn (Message Nothing NICK (Parameters ["caconym_test"] Nothing))

  putStrLn "Press enter to quit"
  getLine

  cancel listenAsync
  sendMessage conn (Message Nothing QUIT (Parameters [] Nothing))
  close conn

listen :: Connection -> IO ()
listen conn@Connection{..} = do
  msg <- readChan incomingMessages
  case msg of
    LostConnection -> debugM rootLoggerName "Failed to read from server"
    Message _ PING params -> sendMessage (Message Nothing PONG params) >> listen conn
    _ -> listen conn
