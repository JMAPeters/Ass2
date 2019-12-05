import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.IORef
import System.Environment
import System.IO
import Network.Socket

readCommandLineArguments :: IO (Int, [Int])
readCommandLineArguments = do
  args <- getArgs
  case args of
    [] -> error "Not enough arguments. You should pass the port number of the current process and a list of neighbours"
    (me:neighbours) -> return (read me, map read neighbours)

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

listenForConnections :: Socket -> IO ()
listenForConnections serverSocket = do
  (connection, _) <- accept serverSocket      -- accept a connection and handle it
  _ <- forkIO $ handleConnection connection   -- do server logic
  listenForConnections serverSocket           -- repeat

handleConnection :: Socket -> IO ()
handleConnection connection = do
  putStrLn "Got new incomming connection"
  chandle <- socketToHandle connection ReadWriteMode
  hPutStrLn chandle "Welcome"
  message <- hGetLine chandle
  putStrLn $ "Incomming connection send a message: " ++ message
  hClose chandle