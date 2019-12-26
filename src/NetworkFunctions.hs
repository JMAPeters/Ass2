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
import Data.List (sortBy, delete)

-- initialisation
------------------------------------------------------------------------
makeConnection :: [Port] -> HashMap Port (IO Handle)
makeConnection neighbours = fromList(zip neighbours connections)
    where connections = Prelude.map connectTo neighbours

connectTo :: Port -> IO Handle
connectTo neighbour = do
  client <- connectSocket neighbour
  connectionClient <- socketToHandle client ReadWriteMode
  return connectionClient

makeRoutingTable :: [Port] -> HashMap Port Path -> HashMap Port Path
makeRoutingTable [neighbour] routingTable = insert neighbour Udef routingTable
makeRoutingTable (neighbour:neighbours) routingTable = makeRoutingTable neighbours (insert neighbour Udef routingTable)

makeEstDist :: [Port] -> Int -> HashMap Port Int -> HashMap Port Int
makeEstDist [neighbour] dist estDist = insert neighbour dist estDist
makeEstDist (neighbour:neighbours) dist estDist = makeEstDist neighbours dist (insert neighbour dist estDist)

--------------------------------------------------------------------


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
  connectionClient <- socketToHandle connection ReadWriteMode
  messageClient <- hGetLine connectionClient
  let (port, func, message) = parseMessage messageClient
  case func of 
    "mydist" -> processDist node lock port message
    "B" -> processMessage node lock func port (concat message)
    "repair" -> processRepair node lock port
    "disconnect" -> processDisconnect node lock port
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
    recompute node portNumber
    interlocked lock "Unlock"


recompute :: Node -> Port -> IO ()
recompute node portNumber = do
  if (portNumber /= (me node)) then do
      _allNodes <- readIORef (allNodes node)
      ((neighNode, _), dist) <- getMinEstDistNeigh portNumber _allNodes (estDistNeigh node)
      let d = 1 + dist
      _estDist <- readIORef (estDist node)
      if (d < (length _allNodes)) then do
          --when (d /= (_estDist ! portNumber)) $ do
            _routingTable <- readIORef (routingTable node)
            writeIORef (routingTable node) (insert portNumber (Portnumber neighNode) _routingTable)
            writeIORef (estDist node) (insert portNumber d _estDist)
            putStrLn ("Distance to " ++ show portNumber ++ " is now " ++ show d ++ " via " ++ show neighNode)
      else do
          _routingTable <- readIORef (routingTable node)
          writeIORef (routingTable node) (insert portNumber Udef _routingTable)
          writeIORef (estDist node) (insert portNumber (length _allNodes) _estDist)
          putStrLn ("Unreachable: " ++ show portNumber)
      
      when (d /= (_estDist ! portNumber) && (d <= (length _allNodes))) $ do
        _neighConnection <- readIORef (neighConnection node)
        let _neighConnection_ = (elems _neighConnection)
        forM_  _neighConnection_ $ \_neigh -> do  
            _neigh_ <- _neigh
            hPutStrLn _neigh_ (show (me node) ++ " mydist " ++ show portNumber ++ " " ++ show d)
  else do
    _neighConnection <- readIORef (neighConnection node)
    let _neighConnection_ = (elems _neighConnection)
    forM_  _neighConnection_ $ \_neigh -> do  
        _neigh_ <- _neigh
        hPutStrLn _neigh_ (show (me node) ++ " mydist " ++ show portNumber ++ " " ++ "0")

getMinEstDistNeigh :: Port -> [Int] -> IORef (HashMap (Port, Port) Int) -> IO ((Port,Port), Int)
getMinEstDistNeigh portNumber allNodes estDistNeigh = do
    _estDistNeigh <- readIORef estDistNeigh
    let list = toList _estDistNeigh
        portList = Prelude.filter ((==portNumber).snd.fst) list
        minList = sortBy (comparing $ snd) portList
        tup = getTup allNodes minList
    return tup

getTup :: [Int] -> [((Port, Port), Int)] -> ((Port, Port), Int)
getTup allNodes [] = ((0,0), (length allNodes))
getTup _ [x] = x
getTup _ (x:_) = x

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
          putStrLn ("Message for " ++ show port ++ " is relayed to " ++ show neigh)
          hPutStrLn neighCon ((show port) ++ " " ++ command ++ " " ++ message)
        else
            putStrLn "Port number is not known"
    interlocked lock "Unlock"

processRepair :: Node -> Lock -> Port -> IO()
processRepair node lock portNumber = do
    interlocked lock "Lock"
    _neighbours <- readIORef (neighbours node)
    writeIORef (neighbours node) (portNumber:_neighbours)
    _neighConnection <- readIORef (neighConnection node)
    writeIORef (neighConnection node) (insert portNumber (connectTo portNumber) _neighConnection)
    _allNodes <- readIORef (allNodes node)
    forM_ _allNodes $ \_node -> do
        _estDistNeigh <- readIORef (estDistNeigh node)
        writeIORef (estDistNeigh node) (insert (portNumber, _node) (length _allNodes) _estDistNeigh)
        _neighConnection <- readIORef (neighConnection node)
        _neighCon <- (_neighConnection ! portNumber)
        _estDist <- readIORef (estDist node)
        hPutStrLn _neighCon (show (me node) ++ " mydist " ++ show _node ++ " " ++ (show (_estDist ! _node)))
    interlocked lock "Unlock"

processDisconnect :: Node -> Lock -> Port -> IO()
processDisconnect node lock portNumber = do
  interlocked lock "Lock"
  _neighbours <- readIORef (neighbours node)
  writeIORef (neighbours node) (Data.List.delete portNumber _neighbours)
  _neighConnection <- readIORef (neighConnection node)
  writeIORef (neighConnection node) (Data.HashMap.Lazy.delete portNumber _neighConnection)

  _allNodes <- readIORef (allNodes node)
  forM_ _allNodes $ \_node -> do
      _estDistNeigh <- readIORef (estDistNeigh node)
      writeIORef (estDistNeigh node) (filterHashMap portNumber _estDistNeigh)
      recompute node _node
  interlocked lock "Unlock"

filterHashMap :: Port -> HashMap (Port, Port) Int -> HashMap (Port, Port) Int
filterHashMap portNumber = filterWithKey (\(portNum, _) _ -> portNum /= portNumber)
