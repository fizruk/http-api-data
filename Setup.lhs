\begin{code}
{-# LANGUAGE CPP #-}
#ifndef MIN_VERSION_Cabal
#define MIN_VERSION_Cabal(x,y,z) 0
#endif
module Main (main) where

import Data.List ( nub )
import Distribution.Package ( InstalledPackageId )
import Distribution.Package ( PackageId, Package (..), packageVersion, packageName )
import Distribution.PackageDescription ( PackageDescription(), TestSuite(..) )
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.Utils ( rewriteFile, createDirectoryIfMissingVerbose, copyFiles )
import Distribution.Simple.BuildPaths ( autogenModulesDir )
import Distribution.Simple.Setup ( BuildFlags(buildVerbosity), Flag(..), fromFlag, HaddockFlags(haddockDistPref))
import Distribution.Simple.LocalBuildInfo ( withLibLBI, withTestLBI, LocalBuildInfo(), ComponentLocalBuildInfo(componentPackageDeps), compiler, buildDir )
import Distribution.Simple.Compiler ( showCompilerId )
import Distribution.Text ( display , simpleParse )
import Distribution.Verbosity ( Verbosity, normal )
import System.FilePath ( (</>) )

#if MIN_VERSION_Cabal(1,25,0)
import Distribution.Simple.BuildPaths ( autogenComponentModulesDir )
#endif

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
     generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi
     buildHook simpleUserHooks pkg lbi hooks flags
  }

haddockOutputDir :: Package p => HaddockFlags -> p -> FilePath
haddockOutputDir flags pkg = destDir where
  baseDir = case haddockDistPref flags of
    NoFlag -> "."
    Flag x -> x
  destDir = baseDir </> "doc" </> "html" </> display (packageName pkg)

generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule verbosity pkg lbi = do
  let dir = autogenModulesDir lbi
  let bdir = buildDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  withLibLBI pkg lbi $ \_ libcfg -> do
    -- We need the directory with library's cabal_macros.h!
#if MIN_VERSION_Cabal(1,25,0)
    let dir2 = autogenComponentModulesDir lbi libcfg
#else
    let dir2 = dir
#endif
    withTestLBI pkg lbi $ \suite suitecfg -> do
      -- when (testName suite == "doctest") $ we need IsString instance
      rewriteFile (dir </> "Build_doctests.hs") $ unlines
        [ "module Build_doctests where"
        , ""
        , "autogen_dir :: String"
        , "autogen_dir = " ++ show dir
        , ""
        , "component_autogen_dir :: String"
        , "component_autogen_dir = " ++ show dir2
        , ""
        , "build_dir :: String"
        , "build_dir = " ++ show bdir
        , ""
        -- -package-id etc. flags
        , "pkgs :: [String]"
        , "pkgs = " ++ (show $ formatDeps $ testDeps libcfg suitecfg)
        , ""
        , "compiler :: String"
        , "compiler = " ++ (show $ showCompilerId $ compiler lbi)
        , ""
        , "isOldCompiler :: Bool"
        , "isOldCompiler = " ++ show isOldCompiler
        ]
  where
    -- we do this check in Setup, as then doctests don't need to depend on Cabal
    isOldCompiler = maybe False id $ do
      a <- simpleParse $ showCompilerId $ compiler lbi
      b <- simpleParse "7.5"
      return $ packageVersion (a :: PackageId) < b

    formatDeps = map formatOne
    formatOne (installedPkgId, pkgId)
      -- The problem is how different cabal executables handle package databases
      -- when doctests depend on the library
      | packageId pkg == pkgId = "-package=" ++ display pkgId
      | otherwise              = "-package-id=" ++ display installedPkgId

testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [(InstalledPackageId, PackageId)]
testDeps xs ys = nub $ componentPackageDeps xs ++ componentPackageDeps ys

\end{code}
