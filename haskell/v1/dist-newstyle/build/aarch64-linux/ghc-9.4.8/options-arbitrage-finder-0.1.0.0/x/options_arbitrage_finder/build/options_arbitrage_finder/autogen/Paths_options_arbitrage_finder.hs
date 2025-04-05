{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_options_arbitrage_finder (
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
bindir     = "/home/tyler/.cabal/bin"
libdir     = "/home/tyler/.cabal/lib/aarch64-linux-ghc-9.4.8/options-arbitrage-finder-0.1.0.0-inplace-options_arbitrage_finder"
dynlibdir  = "/home/tyler/.cabal/lib/aarch64-linux-ghc-9.4.8"
datadir    = "/home/tyler/.cabal/share/aarch64-linux-ghc-9.4.8/options-arbitrage-finder-0.1.0.0"
libexecdir = "/home/tyler/.cabal/libexec/aarch64-linux-ghc-9.4.8/options-arbitrage-finder-0.1.0.0"
sysconfdir = "/home/tyler/.cabal/etc"

getBinDir     = catchIO (getEnv "options_arbitrage_finder_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "options_arbitrage_finder_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "options_arbitrage_finder_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "options_arbitrage_finder_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "options_arbitrage_finder_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "options_arbitrage_finder_sysconfdir") (\_ -> return sysconfdir)



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
