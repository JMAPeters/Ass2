{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_netchange (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "D:\\Doucmenten\\Universiteit\\Concurrency\\Jaar 3\\Ass2\\.stack-work\\install\\8bd22936\\bin"
libdir     = "D:\\Doucmenten\\Universiteit\\Concurrency\\Jaar 3\\Ass2\\.stack-work\\install\\8bd22936\\lib\\x86_64-windows-ghc-8.6.5\\netchange-0.1.0.0-HpQ7uPO5Dau3Y1lRfCjl7g-netchange"
dynlibdir  = "D:\\Doucmenten\\Universiteit\\Concurrency\\Jaar 3\\Ass2\\.stack-work\\install\\8bd22936\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "D:\\Doucmenten\\Universiteit\\Concurrency\\Jaar 3\\Ass2\\.stack-work\\install\\8bd22936\\share\\x86_64-windows-ghc-8.6.5\\netchange-0.1.0.0"
libexecdir = "D:\\Doucmenten\\Universiteit\\Concurrency\\Jaar 3\\Ass2\\.stack-work\\install\\8bd22936\\libexec\\x86_64-windows-ghc-8.6.5\\netchange-0.1.0.0"
sysconfdir = "D:\\Doucmenten\\Universiteit\\Concurrency\\Jaar 3\\Ass2\\.stack-work\\install\\8bd22936\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "netchange_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "netchange_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "netchange_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "netchange_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "netchange_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "netchange_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
