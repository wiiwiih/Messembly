{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Messer (
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

bindir     = "C:\\Users\\vijulapp\\Documents\\GitHub\\Messembly\\LexerParser\\Messer\\.stack-work\\install\\4ce01a7c\\bin"
libdir     = "C:\\Users\\vijulapp\\Documents\\GitHub\\Messembly\\LexerParser\\Messer\\.stack-work\\install\\4ce01a7c\\lib\\x86_64-windows-ghc-8.6.3\\Messer-0.1.0.0-Gm7rgtTydiEHjGrkrFaboT-Messer"
dynlibdir  = "C:\\Users\\vijulapp\\Documents\\GitHub\\Messembly\\LexerParser\\Messer\\.stack-work\\install\\4ce01a7c\\lib\\x86_64-windows-ghc-8.6.3"
datadir    = "C:\\Users\\vijulapp\\Documents\\GitHub\\Messembly\\LexerParser\\Messer\\.stack-work\\install\\4ce01a7c\\share\\x86_64-windows-ghc-8.6.3\\Messer-0.1.0.0"
libexecdir = "C:\\Users\\vijulapp\\Documents\\GitHub\\Messembly\\LexerParser\\Messer\\.stack-work\\install\\4ce01a7c\\libexec\\x86_64-windows-ghc-8.6.3\\Messer-0.1.0.0"
sysconfdir = "C:\\Users\\vijulapp\\Documents\\GitHub\\Messembly\\LexerParser\\Messer\\.stack-work\\install\\4ce01a7c\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Messer_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Messer_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Messer_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Messer_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Messer_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Messer_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
