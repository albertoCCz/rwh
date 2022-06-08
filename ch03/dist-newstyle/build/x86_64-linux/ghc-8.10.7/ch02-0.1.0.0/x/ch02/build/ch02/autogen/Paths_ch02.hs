{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_ch02 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/albertoccz/.cabal/bin"
libdir     = "/home/albertoccz/.cabal/lib/x86_64-linux-ghc-8.10.7/ch02-0.1.0.0-inplace-ch02"
dynlibdir  = "/home/albertoccz/.cabal/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/albertoccz/.cabal/share/x86_64-linux-ghc-8.10.7/ch02-0.1.0.0"
libexecdir = "/home/albertoccz/.cabal/libexec/x86_64-linux-ghc-8.10.7/ch02-0.1.0.0"
sysconfdir = "/home/albertoccz/.cabal/etc"

getBinDir     = catchIO (getEnv "ch02_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "ch02_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "ch02_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "ch02_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ch02_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ch02_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
