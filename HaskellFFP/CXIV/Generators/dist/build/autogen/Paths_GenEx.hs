module Paths_GenEx (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/andres/GitHub/WYaS-Haskell/HaskellFFP/CXIV/Generators/.cabal-sandbox/bin"
libdir     = "/home/andres/GitHub/WYaS-Haskell/HaskellFFP/CXIV/Generators/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/GenEx-0.1.0.0-JE4R0SX9NpgFLGKZIeG075"
datadir    = "/home/andres/GitHub/WYaS-Haskell/HaskellFFP/CXIV/Generators/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/GenEx-0.1.0.0"
libexecdir = "/home/andres/GitHub/WYaS-Haskell/HaskellFFP/CXIV/Generators/.cabal-sandbox/libexec"
sysconfdir = "/home/andres/GitHub/WYaS-Haskell/HaskellFFP/CXIV/Generators/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "GenEx_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "GenEx_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "GenEx_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "GenEx_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "GenEx_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
