{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_failure (
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
version = Version [0,1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/root/project/.stack-work/install/x86_64-linux-tinfo6/lts-12.6/8.4.3/bin"
libdir     = "/root/project/.stack-work/install/x86_64-linux-tinfo6/lts-12.6/8.4.3/lib/x86_64-linux-ghc-8.4.3/failure-0.1.0-DtEio5zznTq46sCCwsuI3F"
dynlibdir  = "/root/project/.stack-work/install/x86_64-linux-tinfo6/lts-12.6/8.4.3/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/root/project/.stack-work/install/x86_64-linux-tinfo6/lts-12.6/8.4.3/share/x86_64-linux-ghc-8.4.3/failure-0.1.0"
libexecdir = "/root/project/.stack-work/install/x86_64-linux-tinfo6/lts-12.6/8.4.3/libexec/x86_64-linux-ghc-8.4.3/failure-0.1.0"
sysconfdir = "/root/project/.stack-work/install/x86_64-linux-tinfo6/lts-12.6/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "failure_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "failure_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "failure_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "failure_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "failure_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "failure_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
