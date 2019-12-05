module Main where
import Functions

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.IORef
import System.Environment
import System.IO
import Network.Socket

-- https://wiki.haskell.org/Implement_a_chat_server


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  -- me :: Int is the port number of this process
  -- neighbours :: [Int] is a list of the port numbers of the initial neighbours
  -- During the execution, connections may be broken or constructed
  (me, neighbours) <- readCommandLineArguments

  putStrLn $ "I should be listening on port " ++ show me
  putStrLn $ "My initial neighbours are " ++ show neighbours

  -- Listen to the specified port.
  serverSocket <- socket AF_INET Stream 0     -- create socket
  setSocketOption serverSocket ReuseAddr 1    -- make socket reusable
  bind serverSocket $ portToAddress me        -- listen on 'me' port
  listen serverSocket 1024                    -- set a max of 1024 queued connections
  -- Let a seperate thread listen for incomming connections
  _ <- forkIO $ listenForConnections serverSocket

  -- As an example, connect to the first neighbour. This just
  -- serves as an example on using the network functions in Haskell
  case neighbours of
    [] -> putStrLn "I have no neighbours :("
    neighbour : _ -> do
      putStrLn $ "Connecting to neighbour " ++ show neighbour ++ "..."
      client <- connectSocket neighbour
      chandle <- socketToHandle client ReadWriteMode
      -- Send a message over the socket
      -- You can send and receive messages with a similar API as reading and writing to the console.
      -- Use `hPutStrLn chandle` instead of `putStrLn`,
      -- and `hGetLine  chandle` instead of `getLine`.
      -- You can close a connection with `hClose chandle`.

      -------------------------------------------------------------------------------
      hPutStrLn chandle $ "Hi process " ++ show neighbour ++ "! I'm process " ++ show me ++ " and you are my first neighbour."
      putStrLn "I sent a message to the neighbour"
      message <- hGetLine chandle
      putStrLn $ "Neighbour send a message back: " ++ show message
      hClose chandle

  threadDelay 1000000000