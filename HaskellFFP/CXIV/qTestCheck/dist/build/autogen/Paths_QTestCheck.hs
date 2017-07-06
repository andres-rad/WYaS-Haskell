module Paths_QTestCheck (
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

bindir     = "/home/andres/GitHub/WYaS-Haskell/HaskellFFP/CXIV/qTestCheck/.cabal-sandbox/bin"
libdir     = "/home/andres/GitHub/WYaS-Haskell/HaskellFFP/CXIV/qTestCheck/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/QTestCheck-0.1.0.0-KLnhXkFFw486aGW6Nh8tGL"
datadir    = "/home/andres/GitHub/WYaS-Haskell/HaskellFFP/CXIV/qTestCheck/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/QTestCheck-0.1.0.0"
libexecdir = "/home/andres/GitHub/WYaS-Haskell/HaskellFFP/CXIV/qTestCheck/.cabal-sandbox/libexec"
sysconfdir = "/home/andres/GitHub/WYaS-Haskell/HaskellFFP/CXIV/qTestCheck/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "QTestCheck_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "QTestCheck_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "QTestCheck_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "QTestCheck_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "QTestCheck_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
