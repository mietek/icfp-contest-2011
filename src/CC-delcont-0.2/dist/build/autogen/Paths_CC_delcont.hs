module Paths_CC_delcont (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,2], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/mietek/.cabal/bin"
libdir     = "/Users/mietek/.cabal/lib/CC-delcont-0.2/ghc-7.0.3"
datadir    = "/Users/mietek/.cabal/share/CC-delcont-0.2"
libexecdir = "/Users/mietek/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "CC_delcont_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "CC_delcont_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "CC_delcont_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "CC_delcont_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
