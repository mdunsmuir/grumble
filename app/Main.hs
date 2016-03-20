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
  sendMessage conn (NICK "caconym_test")

  putStrLn "Press enter to quit"
  getLine

  cancel listenAsync
  close conn

listen :: Connection -> IO ()
listen conn@Connection{..} = do
  msg <- readChan incomingMessages
  case msg of
    ReadFailure -> debugM rootLoggerName "Failed to read from server"
    PING str -> sendMessage (PONG str) >> listen conn
    _ -> listen conn
