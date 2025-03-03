--------------------------------------------------------------------------------
--- Contains functions that access and modify the runtime package cache.
--------------------------------------------------------------------------------

module CPM.PackageCache.Runtime
  ( dependencyPathsSeparate
  , dependencyPaths
  , copyPackages
  , cacheDirectory
  , writePackageConfig
  ) where

import Data.List          ( intercalate, isPrefixOf, split )
import System.Directory   ( createDirectoryIfMissing, copyFile, doesFileExist
                          , getDirectoryContents, doesDirectoryExist )
import System.FilePath    ( (</>), (<.>), splitSearchPath, takeDirectory )

import CPM.Config         ( Config, binInstallDir )
import CPM.ErrorLogger
import CPM.PackageCache.Global (installedPackageDir)
import CPM.Package        ( Package, packageId, PackageExecutable(..)
                          , sourceDirsOf, executableSpec, version
                          , configModule, showVersion )
import CPM.FileUtil       ( copyDirectoryFollowingSymlinks, getRealPath
                          , recreateDirectory )
import CPM.PackageCache.Local as LocalCache
import CPM.Repository     ( readPackageFromRepository )

-- Each package needs its own copy of all dependencies since KiCS2 and PACKS
-- store their intermediate results for each source file in a hidden directory
-- alongside that particular source file. This module manages these local
-- copies.

--- Returns a colon-separated list of the paths to a list of given packages
--- inside a package's runtime package cache.
dependencyPaths :: [Package] -> String -> String
dependencyPaths pkgs dir = intercalate ":" $ dependencyPathsSeparate pkgs dir

--- Returns a list of the paths to a list of given packages inside a package's
--- runtime package cache.
dependencyPathsSeparate :: [Package] -> String -> [String]
dependencyPathsSeparate pkgs dir =
  concatMap (\p -> map (cacheDirectory dir p </>) (sourceDirsOf p)) pkgs

--- Returns the directory for a package inside another package's runtime cache.
cacheDirectory :: String -> Package -> String
cacheDirectory dir pkg = dir </> ".cpm" </> "packages" </> packageId pkg

--- Copies a set of packages from the local package cache to the runtime
--- package cache and returns the package specifications.
copyPackages :: Config -> [Package] -> String -> ErrorLogger [Package]
copyPackages cfg pkgs dir = mapM copyPackage pkgs
  where
    copyPackage pkg = do
      cdir <- ensureCacheDirectory dir
      let destDir = cdir </> packageId pkg
      liftIOEL $ recreateDirectory destDir
      pkgDirExists <- liftIOEL $ doesDirectoryExist pkgDir
      if pkgDirExists
        then do
          -- in order to obtain complete package specification:
          reppkg <- readPackageFromRepository cfg pkg
          liftIOEL $ copyDirectoryFollowingSymlinks pkgDir cdir
          writePackageConfig cfg destDir reppkg ""
          return reppkg
        else error $ "Package " ++ packageId pkg ++
                     " could not be found in package cache."
     where
      pkgDir = LocalCache.packageDir dir pkg

--- Ensures that the runtime package cache directory exists.
ensureCacheDirectory :: String -> ErrorLogger String
ensureCacheDirectory dir = do
  let packagesDir = dir </> ".cpm" </> "packages"
  liftIOEL $ createDirectoryIfMissing True packagesDir
  return packagesDir


--- Writes the package configuration module (if specified) into the
--- the package sources.
writePackageConfig :: Config -> String -> Package -> String -> ErrorLogger ()
writePackageConfig cfg pkgdir pkg loadpath =
  maybe (return ())
        (\configmod ->
           if null configmod
             then return ()
             else do
               let bins = map (\ (PackageExecutable n _ _) -> n)
                                  (executableSpec pkg)
               realbins <-
                 mapM (liftIOEL . getRealPath . (binInstallDir cfg </>)) bins
               writeConfigFile configmod realbins)
        (configModule pkg)
 where
  writeConfigFile configmod binnames = do
    let configfile = pkgdir </> "src" </> foldr1 (</>) (split (=='.') configmod)
                            <.> ".curry"
    liftIOEL $ do
      createDirectoryIfMissing True (takeDirectory configfile)
      abspkgdir <- getRealPath pkgdir
      writeFile configfile $ unlines $
        [ "module " ++ configmod ++ " where"
        , ""
        , "--- Package version as a string."
        , "packageVersion :: String"
        , "packageVersion = \"" ++ showVersion (version pkg) ++ "\""
        , ""
        , "--- Returns the location (installation directory) of the package."
        , "getPackagePath :: IO String"
        , "getPackagePath = return " ++ show abspkgdir
        , ""
        , "--- Package location (deprecated, use 'getPackagePath')."
        , "packagePath :: String"
        , "packagePath = " ++ show abspkgdir
        , ""
        , "--- Returns the load path for the package (if it is the main package)."
        , "getPackageLoadPath :: IO [String]"
        , "getPackageLoadPath = do" ] ++
        (let (slpath,usepp) = showLoadPath abspkgdir
         in if usepp then [ "  pp <- getPackagePath"
                          , "  return " ++ slpath ]
                     else [ "  return " ++ slpath ]) ++
        [ ""
        , "--- Load path for the package (deprecated, use 'getPackageLoadPath')."
        , "packageLoadPath :: String"
        , "packageLoadPath = " ++ show loadpath
        , "" ] ++
        showExecutables binnames
    logDebug $ "Config module '" ++ configfile ++ "' written."

  showLoadPath pdir =
    let (lppath,usepp) = unzip
                           (map replacePackagePath (splitSearchPath loadpath))
    in ("[" ++ intercalate ", " lppath ++ "]", or usepp)
   where
    replacePackagePath d =
      if pdir `isPrefixOf` d
        then ("pp ++ " ++ show (drop (length pdir) d), True)
        else (show d, False)

  showExecutables bins = case length bins of
    0 -> []
    1 -> [ "--- Returns the location of the executable installed by this package."
         , "getPackageExecutable :: IO String"
         , "getPackageExecutable = return \"" ++ head bins ++ "\""
         , ""
         , "--- Location of the executable (deprecated, use 'getPackageExecutable')."
         , "packageExecutable :: String"
         , "packageExecutable = \"" ++ head bins ++ "\""
         ]
    _ -> [ "--- Returns the locations of the executables installed by this package."
         , "getPackageExecutables :: IO [String]"
         , "getPackageExecutables = return [" ++
           intercalate ", " (map (\s -> "\"" ++ s ++ "\"") bins) ++ "]"
         , ""
         , "--- Location of the executables (deprecated, use 'getPackageExecutables')."
         , "packageExecutables :: [String]"
         , "packageExecutables = [" ++
           intercalate ", " (map (\s -> "\"" ++ s ++ "\"") bins) ++ "]"
         ]
