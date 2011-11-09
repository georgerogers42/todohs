module Paths_todohs (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/george/todo/todohs/cabal-dev//bin"
libdir     = "/home/george/todo/todohs/cabal-dev//lib/todohs-0.0.0/ghc-7.0.3"
datadir    = "/home/george/todo/todohs/cabal-dev//share/todohs-0.0.0"
libexecdir = "/home/george/todo/todohs/cabal-dev//libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "todohs_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "todohs_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "todohs_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "todohs_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
