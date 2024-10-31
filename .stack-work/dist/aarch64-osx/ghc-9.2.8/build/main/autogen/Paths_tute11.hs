{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_tute11 (
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
bindir     = "/Users/fat007/Desktop/Pers/wordle/mywordle/.stack-work/install/aarch64-osx/28faa4a56e6334a0aa90a8417a5bbc84fdfedb37da47ee7aa2d17b93075d153f/9.2.8/bin"
libdir     = "/Users/fat007/Desktop/Pers/wordle/mywordle/.stack-work/install/aarch64-osx/28faa4a56e6334a0aa90a8417a5bbc84fdfedb37da47ee7aa2d17b93075d153f/9.2.8/lib/aarch64-osx-ghc-9.2.8/tute11-0.1.0.0-7Ito8p5zSHIJDuxjd3ERmC-main"
dynlibdir  = "/Users/fat007/Desktop/Pers/wordle/mywordle/.stack-work/install/aarch64-osx/28faa4a56e6334a0aa90a8417a5bbc84fdfedb37da47ee7aa2d17b93075d153f/9.2.8/lib/aarch64-osx-ghc-9.2.8"
datadir    = "/Users/fat007/Desktop/Pers/wordle/mywordle/.stack-work/install/aarch64-osx/28faa4a56e6334a0aa90a8417a5bbc84fdfedb37da47ee7aa2d17b93075d153f/9.2.8/share/aarch64-osx-ghc-9.2.8/tute11-0.1.0.0"
libexecdir = "/Users/fat007/Desktop/Pers/wordle/mywordle/.stack-work/install/aarch64-osx/28faa4a56e6334a0aa90a8417a5bbc84fdfedb37da47ee7aa2d17b93075d153f/9.2.8/libexec/aarch64-osx-ghc-9.2.8/tute11-0.1.0.0"
sysconfdir = "/Users/fat007/Desktop/Pers/wordle/mywordle/.stack-work/install/aarch64-osx/28faa4a56e6334a0aa90a8417a5bbc84fdfedb37da47ee7aa2d17b93075d153f/9.2.8/etc"

getBinDir     = catchIO (getEnv "tute11_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "tute11_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "tute11_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "tute11_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tute11_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tute11_sysconfdir") (\_ -> return sysconfdir)




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
