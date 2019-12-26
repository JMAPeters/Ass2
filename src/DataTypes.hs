module DataTypes where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.IORef
import System.Environment
import System.IO
import Network.Socket
import Data.HashMap.Lazy

data Node = Node {
            me :: Port                                           -- portnumber
          , neighbours :: IORef [Port]                                 -- list of portnumbers
          , allNodes :: IORef [Port]                             -- list of all nodes
          , neighConnection :: IORef (HashMap Port (IO Handle))  -- connection to neighbours
          , routingTable :: IORef (HashMap Port Path)            -- hashmap of portnumber and path (path is local/ portnumber/ udef)
          , estDist :: IORef (HashMap Port Int)                  -- hashmap of portnumber and distance
          , estDistNeigh :: IORef (HashMap (Port, Port) Int)     -- hashmap of (portnumber, portnumber) and distance
}

makeNode :: (Port, IORef [Port], IORef [Port], IORef (HashMap Port (IO Handle)), IORef (HashMap Port Path), IORef (HashMap Port Int), IORef (HashMap (Port, Port) Int)) -> Node
makeNode (me, neighbours, allNodes, neighConnection, routingTable, estDist, estDistNeigh) =
    Node me neighbours allNodes neighConnection routingTable estDist estDistNeigh

type Port = Int    
data Path = Local | Portnumber Port | Udef deriving (Eq)
instance Show Path where
    show (Local) = "local"
    show (Portnumber port) = (show port)
    show (Udef) = "udef"
-- instance Eq Path where
--     (==) Local Local = True
--     (==) (Portnumber port) (Portnumber port) = True
--     (==) Udef Udef = True

---------------------------------------------------------------
data Lock = MVarLock (MVar ())

getLock = do
    _lock <- newEmptyMVar
    return (MVarLock _lock)

interlocked :: Lock -> String -> IO ()
interlocked (MVarLock _lock) "Lock" = do
    putMVar _lock ()
interlocked (MVarLock _lock) "Unlock" = do
    takeMVar _lock
    return ()
----------------------------------------------------------------