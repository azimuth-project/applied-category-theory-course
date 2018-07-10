{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_applied_category_theory_course (
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

bindir     = "/Volumes/First/Projects/applied-category-theory-course/.stack-work/install/x86_64-osx/lts-11.15/8.2.2/bin"
libdir     = "/Volumes/First/Projects/applied-category-theory-course/.stack-work/install/x86_64-osx/lts-11.15/8.2.2/lib/x86_64-osx-ghc-8.2.2/applied-category-theory-course-0.1.0.0-38ZMTIR4w9iJCdihe3GYC1-applied-category-theory-course-test"
dynlibdir  = "/Volumes/First/Projects/applied-category-theory-course/.stack-work/install/x86_64-osx/lts-11.15/8.2.2/lib/x86_64-osx-ghc-8.2.2"
datadir    = "/Volumes/First/Projects/applied-category-theory-course/.stack-work/install/x86_64-osx/lts-11.15/8.2.2/share/x86_64-osx-ghc-8.2.2/applied-category-theory-course-0.1.0.0"
libexecdir = "/Volumes/First/Projects/applied-category-theory-course/.stack-work/install/x86_64-osx/lts-11.15/8.2.2/libexec/x86_64-osx-ghc-8.2.2/applied-category-theory-course-0.1.0.0"
sysconfdir = "/Volumes/First/Projects/applied-category-theory-course/.stack-work/install/x86_64-osx/lts-11.15/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "applied_category_theory_course_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "applied_category_theory_course_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "applied_category_theory_course_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "applied_category_theory_course_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "applied_category_theory_course_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "applied_category_theory_course_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
