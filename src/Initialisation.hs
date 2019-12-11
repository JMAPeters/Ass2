module Initialisation where
import DataTypes
import NetworkFunctions

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.IORef
import System.Environment
import System.IO
import Network.Socket
import Data.HashMap.Lazy

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

-- makeEstDistNeigh :: [Port] -> Int -> HashMap (Port,Port) Int -> HashMap (Port,Port) Int
-- makeEstDistNeigh [neighbour] dist estDistNeigh = insert (neighbour,neighbour) dist estDistNeigh
-- makeEstDistNeigh (neighbour:neighbours) dist estDistNeigh = makeEstDistNeigh neighbours dist (insert (neighbour,neighbour) dist estDistNeigh)