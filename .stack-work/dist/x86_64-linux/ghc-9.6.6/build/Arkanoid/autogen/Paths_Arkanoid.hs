{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_Arkanoid (
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
bindir     = "/home/vitor/UFABC/ProgramacaoFuncional2024.2/Arkanoid/.stack-work/install/x86_64-linux/c09aa8e24d9aca22d98a72954f63cc30f8c40f2362f61b5936532c4589e67866/9.6.6/bin"
libdir     = "/home/vitor/UFABC/ProgramacaoFuncional2024.2/Arkanoid/.stack-work/install/x86_64-linux/c09aa8e24d9aca22d98a72954f63cc30f8c40f2362f61b5936532c4589e67866/9.6.6/lib/x86_64-linux-ghc-9.6.6/Arkanoid-0.1.0.0-2XxV1rwfVsj92E9cVy6jsk-Arkanoid"
dynlibdir  = "/home/vitor/UFABC/ProgramacaoFuncional2024.2/Arkanoid/.stack-work/install/x86_64-linux/c09aa8e24d9aca22d98a72954f63cc30f8c40f2362f61b5936532c4589e67866/9.6.6/lib/x86_64-linux-ghc-9.6.6"
datadir    = "/home/vitor/UFABC/ProgramacaoFuncional2024.2/Arkanoid/.stack-work/install/x86_64-linux/c09aa8e24d9aca22d98a72954f63cc30f8c40f2362f61b5936532c4589e67866/9.6.6/share/x86_64-linux-ghc-9.6.6/Arkanoid-0.1.0.0"
libexecdir = "/home/vitor/UFABC/ProgramacaoFuncional2024.2/Arkanoid/.stack-work/install/x86_64-linux/c09aa8e24d9aca22d98a72954f63cc30f8c40f2362f61b5936532c4589e67866/9.6.6/libexec/x86_64-linux-ghc-9.6.6/Arkanoid-0.1.0.0"
sysconfdir = "/home/vitor/UFABC/ProgramacaoFuncional2024.2/Arkanoid/.stack-work/install/x86_64-linux/c09aa8e24d9aca22d98a72954f63cc30f8c40f2362f61b5936532c4589e67866/9.6.6/etc"

getBinDir     = catchIO (getEnv "Arkanoid_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "Arkanoid_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "Arkanoid_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "Arkanoid_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Arkanoid_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Arkanoid_sysconfdir") (\_ -> return sysconfdir)



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
