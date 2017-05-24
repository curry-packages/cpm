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

import FilePath    ( (</>), (<.>) )
import FileGoodies ( baseName )
import Directory   ( createDirectoryIfMissing, copyFile, getDirectoryContents
                   , getAbsolutePath, doesDirectoryExist, doesFileExist )
import List        ( intercalate, split )

import CPM.Config (Config, binInstallDir)
import CPM.ErrorLogger
import CPM.PackageCache.Global (installedPackageDir)
import CPM.Package  ( Package, packageId, PackageExecutable(..), sourceDirsOf
                    , configModule, executableSpec, version, showVersion )
import CPM.FileUtil ( copyDirectoryFollowingSymlinks, recreateDirectory )
import CPM.PackageCache.Local as LocalCache

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
--- package cache.
copyPackages :: Config -> [Package] -> String -> IO ()
copyPackages cfg pkgs dir = mapIO copyPackage pkgs >> return ()
  where
    copyPackage pkg = do
      cdir <- ensureCacheDirectory dir
      destDir <- return $ cdir </> (packageId pkg)
      recreateDirectory destDir
      pkgDirExists <- doesDirectoryExist pkgDir
      if pkgDirExists
        then copyDirectoryFollowingSymlinks pkgDir cdir >>
             writePackageConfig cfg destDir pkg |> succeedIO ()
        else error $ "Package " ++ packageId pkg ++
                     " could not be found in package cache." 
     where 
      pkgDir = LocalCache.packageDir dir pkg

--- Ensures that the runtime package cache directory exists.
ensureCacheDirectory :: String -> IO String
ensureCacheDirectory dir = do
  createDirectoryIfMissing True packagesDir
  return packagesDir
    where packagesDir = dir </> ".cpm" </> "packages"


--- Writes the package configuration module (if specified) into the
--- the package sources.
writePackageConfig :: Config -> String -> Package -> IO (ErrorLogger ())
writePackageConfig cfg pkgdir pkg =
  maybe (succeedIO ())
        (\ configmod ->
           let binname = maybe ""
                               (\ (PackageExecutable n _ _) -> n)
                               (executableSpec pkg)
           in if null configmod
                then succeedIO ()
                else writeConfigFile configmod binname)
        (configModule pkg)
 where
  writeConfigFile configmod binname = do
    let configfile = pkgdir </> "src" </> foldr1 (</>) (split (=='.') configmod)
                            <.> ".curry"
    abspkgdir <- getAbsolutePath pkgdir
    writeFile configfile $ unlines $
      [ "module " ++ configmod ++ " where"
      , ""
      , "--- Package version as a string."
      , "packageVersion :: String"
      , "packageVersion = \"" ++ showVersion (version pkg) ++ "\""
      , ""
      , "--- Package location."
      , "packagePath :: String"
      , "packagePath = \"" ++ abspkgdir ++ "\""
      ] ++
      if null binname
      then []
      else [ ""
           , "--- Location of the executable installed by this package."
           , "packageExecutable :: String"
           , "packageExecutable = \"" ++ binInstallDir cfg </> binname ++ "\""
           ]
    log Debug $ "Config module '" ++ configfile ++ "' written."
