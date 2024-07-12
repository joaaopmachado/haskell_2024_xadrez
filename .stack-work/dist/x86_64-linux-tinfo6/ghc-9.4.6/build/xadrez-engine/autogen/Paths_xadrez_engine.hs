{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_xadrez_engine (
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
bindir     = "/home/joao_/23-q2-paradigmas-projetos-team-joao-machado/.stack-work/install/x86_64-linux-tinfo6/cc5237a29e8e9463966e3c3a7452c170c2338f4d29fa764b6d9be9ef9e1b6e38/9.4.6/bin"
libdir     = "/home/joao_/23-q2-paradigmas-projetos-team-joao-machado/.stack-work/install/x86_64-linux-tinfo6/cc5237a29e8e9463966e3c3a7452c170c2338f4d29fa764b6d9be9ef9e1b6e38/9.4.6/lib/x86_64-linux-ghc-9.4.6/xadrez-engine-0.1.0.0-4rV9XkTZs8FJDJlnJ1AymT-xadrez-engine"
dynlibdir  = "/home/joao_/23-q2-paradigmas-projetos-team-joao-machado/.stack-work/install/x86_64-linux-tinfo6/cc5237a29e8e9463966e3c3a7452c170c2338f4d29fa764b6d9be9ef9e1b6e38/9.4.6/lib/x86_64-linux-ghc-9.4.6"
datadir    = "/home/joao_/23-q2-paradigmas-projetos-team-joao-machado/.stack-work/install/x86_64-linux-tinfo6/cc5237a29e8e9463966e3c3a7452c170c2338f4d29fa764b6d9be9ef9e1b6e38/9.4.6/share/x86_64-linux-ghc-9.4.6/xadrez-engine-0.1.0.0"
libexecdir = "/home/joao_/23-q2-paradigmas-projetos-team-joao-machado/.stack-work/install/x86_64-linux-tinfo6/cc5237a29e8e9463966e3c3a7452c170c2338f4d29fa764b6d9be9ef9e1b6e38/9.4.6/libexec/x86_64-linux-ghc-9.4.6/xadrez-engine-0.1.0.0"
sysconfdir = "/home/joao_/23-q2-paradigmas-projetos-team-joao-machado/.stack-work/install/x86_64-linux-tinfo6/cc5237a29e8e9463966e3c3a7452c170c2338f4d29fa764b6d9be9ef9e1b6e38/9.4.6/etc"

getBinDir     = catchIO (getEnv "xadrez_engine_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "xadrez_engine_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "xadrez_engine_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "xadrez_engine_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "xadrez_engine_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "xadrez_engine_sysconfdir") (\_ -> return sysconfdir)




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
