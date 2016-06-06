module Paths_billboard_parser (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1,0,0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/pim/.cabal/bin"
libdir     = "/home/pim/.cabal/lib/billboard-parser-1.0.0.1/ghc-7.6.3"
datadir    = "/home/pim/.cabal/share/billboard-parser-1.0.0.1"
libexecdir = "/home/pim/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "billboard_parser_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "billboard_parser_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "billboard_parser_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "billboard_parser_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
