{-# LANGUAGE CPP #-}

import Control.Monad (foldM_, forM_)
import System.Cmd
import System.Exit
import System.Info (os)
import System.FilePath
import System.Directory ( doesFileExist, copyFile, removeFile, createDirectoryIfMissing )

import Distribution.PackageDescription
import Distribution.Simple.Setup
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo

main :: IO ()
main = do
            putStrLn $ "Setting up wxhnotepad for " ++ os
            defaultMainWithHooks $ addMacHook simpleUserHooks
 where
  addMacHook h =
   case os of
    "darwin" -> h { postInst = appBundleHook }
    _        -> h

appBundleHook :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
appBundleHook _ _ pkg localb =
 forM_ exes $ \app ->
   do createAppBundle theBindir (buildDir localb </> app </> app)
      removeFile (theBindir </> app)
      createAppBundleWrapper theBindir app
      return ()
 where
  theBindir = bindir $ absoluteInstallDirs pkg localb NoCopyDest
  exes = map exeName $ executables pkg

-- ----------------------------------------------------------------------
-- helper code for application bundles
-- ----------------------------------------------------------------------

-- | 'createAppBundle' @d p@ - creates an application bundle in @d@
--   for program @p@, assuming that @d@ already exists and is a directory.
--   Note that only the filename part of @p@ is used.
createAppBundle :: FilePath -> FilePath -> IO ()
createAppBundle dir p =
 do createDirectoryIfMissing False $ bundle
    createDirectoryIfMissing True  $ bundleBin
    createDirectoryIfMissing True  $ bundleRsrc
    copyFile p (bundleBin </> takeFileName p)
 where
  bundle     = appBundlePath dir p
  bundleBin  = bundle </> "Contents/MacOS"
  bundleRsrc = bundle </> "Contents/Resources"

-- | 'createAppBundleWrapper' @d p@ - creates a script in @d@ that calls
--   @p@ from the application bundle @d </> takeFileName p <.> "app"@
createAppBundleWrapper :: FilePath -> FilePath -> IO ExitCode
createAppBundleWrapper bindir p =
  do writeFile scriptFile scriptTxt
     makeExecutable scriptFile
 where
  scriptFile = bindir </> takeFileName p
  scriptTxt = "`dirname $0`" </> appBundlePath "." p </> "Contents/MacOS" </> takeFileName p ++ " \"$@\""

appBundlePath :: FilePath -> FilePath -> FilePath
appBundlePath dir p = dir </> takeFileName p <.> "app"

-- ----------------------------------------------------------------------
-- utilities
-- ----------------------------------------------------------------------

makeExecutable :: FilePath -> IO ExitCode
makeExecutable f = system $ "chmod a+x " ++ f