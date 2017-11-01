{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_vellian (
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
version = Version [0,0,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/x86_64-linux-ghc-8.0.2/vellian-0.0.2-L8NYWTD1ERTKSaQTJIKGng"
dynlibdir  = "/usr/local/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/usr/local/share/x86_64-linux-ghc-8.0.2/vellian-0.0.2"
libexecdir = "/usr/local/libexec"
sysconfdir = "/usr/local/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "vellian_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "vellian_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "vellian_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "vellian_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "vellian_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "vellian_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
