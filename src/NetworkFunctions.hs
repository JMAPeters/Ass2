module NetworkFunctions where
import DataTypes

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.IORef
import System.Environment
import System.IO
import Network.Socket
import Data.HashMap.Lazy
import Data.Ord
import Data.List (sortBy)

readCommandLineArguments :: IO (Int, [Int])
readCommandLineArguments = do
  args <- getArgs
  case args of
    [] -> error "Not enough arguments. You should pass the port number of the current process and a list of neighbours"
    (me:neighbours) -> return (read me, Prelude.map read neighbours)

portToAddress :: Int -> SockAddr
portToAddress portNumber = SockAddrInet (fromIntegral portNumber) (tupleToHostAddress (127, 0, 0, 1)) -- localhost

connectSocket :: Int -> IO Socket
connectSocket portNumber = connect'
  where
    connect' = do
      client <- socket AF_INET Stream 0
      result <- try $ connect client $ portToAddress portNumber
      case result :: Either IOException () of
        Left _ -> do
          threadDelay 1000000
          connect'
        Right _ -> return client

------------------------------------------------------------------------------

listenForConnections :: Socket -> Node -> Lock -> IO ()
listenForConnections serverSocket node lock = do
  (connection, _) <- accept serverSocket                  -- accept a connection and handle it
  _ <- forkIO $ handleConnection connection node lock     -- do server logic
  listenForConnections serverSocket node lock             -- repeat

handleConnection :: Socket -> Node -> Lock -> IO ()
handleConnection connection node lock = do
  -- This part has to be locked
  connectionClient <- socketToHandle connection ReadWriteMode
  messageClient <- hGetLine connectionClient
  let (port, func, message) = parseMessage messageClient
  case func of 
    "mydist" -> processDist node lock port message
    "B" -> processMessage node lock func port (concat message)
    _ -> return ()
  hClose connectionClient
  
parseMessage :: String -> (Port, String, [String])
parseMessage messageClient = (port, func, message)
    where
      port = read (string !! 0) :: Port
      func = string !! 1
      message = (drop 2 string)
      string = words messageClient

processDist :: Node -> Lock -> Port -> [String] -> IO ()
processDist node lock from (portNum:[dist]) = do
    let portNumber = read portNum :: Port
    let distance = read dist :: Int
    interlocked lock "Lock"
    _allNodes <- readIORef (allNodes node)
    -- if incomming node is a new node insert in routingtable and estDist
    when (portNumber `notElem` _allNodes) $ do
        writeIORef (allNodes node) (portNumber:_allNodes)
        _routingTable <- readIORef (routingTable node)
        writeIORef (routingTable node) (insert portNumber Udef _routingTable)
        _estDist <- readIORef (estDist node)
        writeIORef (estDist node) (insert portNumber (length _allNodes) _estDist)
  
    -- if incomming node is in estDistNeigh, update the value, and if not insert in estDistNeigh
    _estDistNeigh <- readIORef (estDistNeigh node)
    -- if the key already exists the value is updated
    writeIORef (estDistNeigh node) (insert (from, portNumber) distance _estDistNeigh)
    _test <- readIORef (estDistNeigh node)
    --print _test
    interlocked lock "Unlock"
    -- recompute

    --Prelude.mapM_ (recompute node lock) _allNodes
    recompute node lock portNumber

recompute :: Node -> Lock -> Port -> IO ()
recompute node lock portNumber = do
    when (portNumber /= (me node)) $ do
        -- 1 + min van alle sdis van alle neighs directe tot huidige node.
        interlocked lock "Lock"
        ((neighNode, _), dist) <- getMinEstDistNeigh portNumber (estDistNeigh node)
        let d = 1 + dist
        --putStrLn ("recompute: " ++ (show (me node)) ++ " " ++ (show portNumber) ++ " " ++ (show neighNode) ++ " " ++ (show dist) ++ " " ++ (show d))
        _allNodes <- readIORef (allNodes node)
        _estDist <- readIORef (estDist node)
        if (d < (length _allNodes)) then do
            --when (d /= (_estDist ! portNumber)) $ do
              _routingTable <- readIORef (routingTable node)
              writeIORef (routingTable node) (insert portNumber (Portnumber neighNode) _routingTable)
              writeIORef (estDist node) (insert portNumber d _estDist)
        else do
            _routingTable <- readIORef (routingTable node)
            writeIORef (routingTable node) (insert portNumber Udef _routingTable)
            writeIORef (estDist node) (insert portNumber (length _allNodes) _estDist)
        interlocked lock "Unlock"

        when (d /= (_estDist ! portNumber)) $ do
          _neighConnection <- readIORef (neighConnection node)
          let _neighConnection_ = (elems _neighConnection)
          forM_  _neighConnection_ $ \_neigh -> do  
              _neigh_ <- _neigh
              hPutStrLn _neigh_ (show (me node) ++ " mydist " ++ show portNumber ++ " " ++ show d)

getMinEstDistNeigh :: Port -> IORef (HashMap (Port, Port) Int) -> IO ((Port,Port), Int)
getMinEstDistNeigh portNumber estDistNeigh = do
    _estDistNeigh <- readIORef estDistNeigh
    let list = toList _estDistNeigh
        portList = Prelude.filter ((==portNumber).snd.fst) list
        minList = sortBy (comparing $ snd) portList
        tup = head minList
    return tup

processMessage :: Node -> Lock -> String -> Port -> String -> IO ()
processMessage node lock command port message = do
    interlocked lock "Lock"
    if ((me node) == port) then do
        putStrLn message
    else do
        _routingTable <- readIORef (routingTable node)
        _neighConnection <- readIORef (neighConnection node)
        if (port `member` _routingTable) then do
            let (Portnumber neigh) = (_routingTable ! port)
            neighCon <- (_neighConnection ! neigh)
            hPutStrLn neighCon ((show port) ++ " " ++ command ++ " " ++ message)
        else
            putStrLn "Port number is not known"
    interlocked lock "Unlock"