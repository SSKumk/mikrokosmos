{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_mikrokosmos (
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
version = Version [0,8,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/serge/.cabal/bin"
libdir     = "/home/serge/.cabal/lib/x86_64-linux-ghc-8.10.2/mikrokosmos-0.8.1-inplace-test"
dynlibdir  = "/home/serge/.cabal/lib/x86_64-linux-ghc-8.10.2"
datadir    = "/home/serge/.cabal/share/x86_64-linux-ghc-8.10.2/mikrokosmos-0.8.1"
libexecdir = "/home/serge/.cabal/libexec/x86_64-linux-ghc-8.10.2/mikrokosmos-0.8.1"
sysconfdir = "/home/serge/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mikrokosmos_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mikrokosmos_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "mikrokosmos_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "mikrokosmos_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mikrokosmos_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mikrokosmos_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
