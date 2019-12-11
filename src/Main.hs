module Main where
import NetworkFunctions
import DataTypes
import Initialisation

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.IORef
import System.Environment
import System.IO
import Network.Socket
import Data.HashMap.Lazy
import Data.List

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  lock <- getLock

  (me, neighbours) <- readCommandLineArguments
  allNodes <- newIORef (me:neighbours)
  neighConnection <- newIORef empty
  routingTable <- newIORef (singleton me Local)
  estDist <- newIORef (singleton me 0)
  estDistNeigh <- newIORef empty
  let node = makeNode (me, neighbours, allNodes, neighConnection, routingTable, estDist, estDistNeigh)

  putStrLn $ "I should be listening on port " ++ show me
  putStrLn $ "My initial neighbours are " ++ show neighbours

  -- Listen to the specified port.
  serverSocket <- socket AF_INET Stream 0     -- create socket
  setSocketOption serverSocket ReuseAddr 1    -- make socket reusable
  bind serverSocket $ portToAddress me        -- listen on 'me' port
  listen serverSocket 1024                    -- set a max of 1024 queued connections

--------------------------------------------------------------------------------
  -- Initialization
  -- make connection with neighbours
  writeIORef neighConnection (makeConnection neighbours)
  -- make routingtable (Nb)
  _routingTable <- readIORef routingTable
  writeIORef routingTable (makeRoutingTable neighbours _routingTable)
  -- make estemated distance (D)
  _estDist <- readIORef estDist
  _allNodes <- readIORef allNodes
  writeIORef estDist (makeEstDist neighbours (length _allNodes) _estDist) -- lenght of neighbour list plus self
  -- make estDistNeigh (ndis) ?????????????????????? 

  -- Let a seperate thread listen for incomming connections
  _ <- forkIO $ listenForConnections serverSocket node lock

  -- Sent initialisation message to all neighbours
  _neighConnection <- readIORef neighConnection
  let _neighConnection_ = (elems _neighConnection)
  forM_  _neighConnection_ $ \_neigh -> do
      _neigh_ <- _neigh
      hPutStrLn _neigh_ (show me ++ " mydist " ++ show me ++ " " ++ show 0)
------------------------------------------------------------------------
  handleInput node lock

  threadDelay 1000000000

-------- end of main -------------

handleInput :: Node -> Lock -> IO ()
handleInput node lock = do
  line <- getLine
  let (command, portNumber, message) = parseInput line
  case (command) of
    ("R") -> do
        interlocked lock "Lock"
        _allNodes <- readIORef (allNodes node)
        mapM_ (printTable node) _allNodes
        interlocked lock "Unlock"
    ("B") -> do
        interlocked lock "Lock"
        _routingTable <- readIORef (routingTable node)
        _neighConnection <- readIORef (neighConnection node)
        if (portNumber `member` _routingTable && portNumber /= (me node)) then do
            let (Portnumber neigh) = (_routingTable ! portNumber)
            neighCon <- (_neighConnection ! neigh)
            hPutStrLn neighCon ((show portNumber) ++ " " ++ command ++ " " ++ message)
        else
            putStrLn "Port number is not known"
        interlocked lock "Unlock"
  handleInput node lock

parseInput :: String -> (String, Port, String)
parseInput input = (func, port, message)
    where
      func = string !! 0
      port = read (string !! 1) :: Port
      message = intercalate " " (drop 2 string)
      string = words input   

printTable :: Node -> Port -> IO ()
printTable node portNumber = do
    _estDist <- readIORef (estDist node)
    let dist = _estDist ! portNumber
    _routingTable <-readIORef (routingTable node)
    let via = _routingTable ! portNumber
    putStrLn ((show portNumber) ++ " " ++ (show dist) ++ " " ++ (show via))