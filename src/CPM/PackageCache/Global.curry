--------------------------------------------------------------------------------
--- This module contains functions for accessing and modifying the global
--- package cache.
--------------------------------------------------------------------------------

module CPM.PackageCache.Global
  ( GlobalCache
  , findAllVersions
  , findNewestVersion
  , findVersion
  , packageInstalled
  , installedPackageDir
  , readGlobalCache, readInstalledPackagesFromDir
  , allPackages
  , copyPackage
  , installMissingDependencies
  , acquireAndInstallPackage
  , tryFindPackage
  , missingPackages
  , installFromZip
  , checkoutPackage
  , uninstallPackage
  , emptyCache
  ) where

import Data.Either
import Data.List
import Data.Maybe       (isJust)
import System.FilePath
import System.Directory
import System.IOExts    ( readCompleteFile )
import Prelude hiding   ( log )

import CPM.Config       ( Config, packageInstallDir )
import CPM.ErrorLogger
import CPM.FileUtil     ( copyDirectory, inTempDir, recreateDirectory
                        , inDirectory, removeDirectoryComplete
                        , tempDir, whenFileExists
                        , checkAndGetVisibleDirectoryContents, quote )
import CPM.Package
import CPM.Package.Helpers ( installPackageSourceTo )
import CPM.Repository

------------------------------------------------------------------------------
--- The data type representing the global package cache.
data GlobalCache = GlobalCache [Package]

--- An empty package cache.
emptyCache :: GlobalCache
emptyCache = GlobalCache []

--- Gets all package specifications from a cache.
allPackages :: GlobalCache -> [Package]
allPackages (GlobalCache ps) = ps

------------------------------------------------------------------------------
--- Finds all versions of a package in the global package cache.
---
--- @param gc - the global package cache
--- @param p - the name of the package
--- @param pre - include pre-release versions
findAllVersions :: GlobalCache -> String -> Bool -> [Package]
findAllVersions (GlobalCache ps) p pre = sortBy pkgGt
  $ filter filterPre
  $ filter ((== p) . name) ps
 where
  filterPre p' = pre || (not . isPreRelease . version) p'

--- Compares two packages by their versions.
pkgGt :: Package -> Package -> Bool
pkgGt a b = version a `vgt` version b

--- Finds the newest version of a package.
findNewestVersion :: GlobalCache -> String -> Maybe Package
findNewestVersion db p = if length pkgs > 0
  then Just $ head pkgs
  else Nothing
 where
  pkgs = sortBy pkgGt $ findAllVersions db p False

--- Finds a specific version of a package.
findVersion :: GlobalCache -> String -> Version -> Maybe Package
findVersion (GlobalCache ps) p v =
  if null hits
    then Nothing
    else Just $ head hits
 where
  hits = filter ((== v) . version) $ filter ((== p) . name) ps

--- Checks whether a package is installed.
isPackageInstalled :: GlobalCache -> Package -> Bool
isPackageInstalled db p = isJust $ findVersion db (name p) (version p)

--- The directory of a package in the global package cache. Does not check
--- whether the package is actually installed!
installedPackageDir :: Config -> Package -> String
installedPackageDir cfg pkg = packageInstallDir cfg </> packageId pkg

--- Checks whether a package is installed in the global cache.
packageInstalled :: Config -> Package -> IO Bool
packageInstalled cfg pkg =
  doesDirectoryExist (installedPackageDir cfg pkg)

--- Copy a package version to a directory.
copyPackage :: Config -> Package -> String -> ErrorLogger ()
copyPackage cfg pkg dir = do
  exists <- liftIOErrorLogger $ doesDirectoryExist srcDir
  if not exists
    then fail $ "Package '" ++ packageId pkg ++ "' not installed"
    else liftIOErrorLogger (copyDirectory srcDir dir) >> return ()
 where
  srcDir = installedPackageDir cfg pkg

--- Acquires a package from the source specified in its specification and
--- installs it to the global package cache.
acquireAndInstallPackage :: Config -> Package -> ErrorLogger ()
acquireAndInstallPackage cfg reppkg =
  readPackageFromRepository cfg reppkg >>= \pkg ->
  case source pkg of
   Nothing -> fail $ "No source specified for " ++ packageId pkg
   Just  s -> log Info ("Installing package '" ++ packageId pkg ++ "'...") >>
              installFromSource cfg pkg s

------------------------------------------------------------------------------
--- Installs a package from the given package source to the global package
--- cache.
installFromSource :: Config -> Package -> PackageSource -> ErrorLogger ()
installFromSource cfg pkg pkgsource = do
  pkgDirExists <- liftIOErrorLogger $ doesDirectoryExist pkgDir
  if pkgDirExists
    then
      log Info $ "Package '" ++ packageId pkg ++ "' already installed, skipping"
    else log Info ("Installing package from " ++ showPackageSource pkg) >>
         installPackageSourceTo pkg pkgsource (packageInstallDir cfg)
 where
  pkgDir = installedPackageDir cfg pkg

--- Installs a package from a ZIP file to the global package cache.
installFromZip :: Config -> String -> ErrorLogger ()
installFromZip cfg zip = do
  t <- liftIOErrorLogger tempDir
  liftIOErrorLogger $ recreateDirectory (t </> "installtmp")
  absZip <- liftIOErrorLogger $ getAbsolutePath zip
  c <- inTempDirEL $ showExecCmd $ "unzip -qq -d installtmp " ++ quote absZip
  if c == 0
    then
      loadPackageSpec (t </> "installtmp") >>= \pkgSpec ->
      log Debug ("ZIP contains " ++ packageId pkgSpec) >>
      installFromSource cfg pkgSpec (FileSource zip)
    else fail "failed to extract ZIP file"

--- Installs a package's missing dependencies.
installMissingDependencies :: Config -> GlobalCache -> [Package]
                           -> ErrorLogger ()
installMissingDependencies cfg gc deps =
  if length missing > 0
    then log Info logMsg >>
         mapM (acquireAndInstallPackage cfg) missing >>
         return ()
    else return ()
 where
   missing = filter (not . isPackageInstalled gc) deps
   logMsg = "Installing missing dependencies " ++
            intercalate "," (map packageId missing)

--- Filters a list of packages to the ones not installed in the global package
--- cache.
missingPackages :: GlobalCache -> [Package] -> [Package]
missingPackages gc = filter (not . isPackageInstalled gc)

--- Checkout a package from the global package cache.
checkoutPackage :: Config -> Package
                -> ErrorLogger ()
checkoutPackage cfg pkg = do
  sexists <- liftIOErrorLogger $ doesDirectoryExist pkgDir
  texists <- liftIOErrorLogger $ doesDirectoryExist codir
  if texists
    then log Error $ "Local package directory '" ++ codir ++ "' already exists."
    else if sexists
           then liftIOErrorLogger (copyDirectory pkgDir codir) >> log Info logMsg
           else log Error $ "Package '" ++ pkgId ++ "' is not installed."
 where
  pkgId  = packageId pkg
  pkgDir = installedPackageDir cfg pkg
  codir  = name pkg
  logMsg = "Package '" ++ pkgId ++ "' checked out into directory '" ++
           codir ++ "'."

--- Removes a package from the global package cache.
uninstallPackage :: Config -> String -> Version -> ErrorLogger ()
uninstallPackage cfg pkg ver = do
  exists <- liftIOErrorLogger $ doesDirectoryExist pkgDir
  if exists
    then showExecCmd ("rm -Rf " ++ quote pkgDir) >> log Info logMsg
    else log Info $ "Package '" ++ pkgId ++ "' is not installed."
 where
  pkgDir = packageInstallDir cfg </> pkgId
  pkgId  = pkg ++ "-" ++ showVersion ver
  logMsg = "Package '" ++ pkgId ++ "' uninstalled."

--- Tries to find a package in the global package cache.
tryFindPackage :: GlobalCache -> String -> Version -> ErrorLogger Package
tryFindPackage gc name ver = case findVersion gc name ver of
  Just pkg -> return pkg
  Nothing -> fail $ "Package " ++ name ++ "-" ++ showVersion ver ++
                      " could not be found."

--- Reads the global package cache.
readGlobalCache :: Config -> Repository -> ErrorLogger GlobalCache
readGlobalCache config repo = do
  maybeGC <- readInstalledPackagesFromDir repo $ packageInstallDir config
  case maybeGC of
    Left err -> fail $ "Error reading global package cache: " ++ err
    Right gc -> return gc

--- Tries to read package specifications from a GC directory structure.
--- If some GC package directory has the same name as a package from
--- the repository index, the package specification from the repository
--- is used, otherwise (this case should not occur) the package specification
--- stored in the directory is read.
--- This should result in faster GC loading.
readInstalledPackagesFromDir :: Repository -> String
                             -> ErrorLogger (Either String GlobalCache)
readInstalledPackagesFromDir repo path = do
  debugMessage $ "Reading global package cache from '" ++ path ++ "'..."
  pkgPaths <- liftIOErrorLogger $ checkAndGetVisibleDirectoryContents path
  specs <- mapM loadPackageSpecFromDir pkgPaths
  if null (lefts specs)
    then do debugMessage "Finished reading global package cache"
            return (Right $ GlobalCache (rights specs))
    else return (Left $ intercalate "; " (lefts specs))
 where
  readPackageSpecIO = liftIOErrorLogger . fmap readPackageSpec

  loadPackageSpecFromDir pkgdir = case packageVersionFromFile pkgdir of
    Nothing -> readPackageSpecFromFile pkgdir
    Just (pn,pv) -> case CPM.Repository.findVersion repo pn pv of
      Nothing -> readPackageSpecFromFile pkgdir
      Just p  -> return (Right p)

  readPackageSpecFromFile pkgdir = do
    let f = path </> pkgdir </> "package.json"
    debugMessage $ "Reading package spec from '" ++ f ++ "'..."
    spec <- readPackageSpecIO $ readCompleteFile f
    return $ case spec of
      Left err -> Left $ err ++ " for file '" ++ f ++ "'"
      Right  v -> Right v

  packageVersionFromFile :: String -> Maybe (String, Version)
  packageVersionFromFile fn =
    let ps = split (=='-') fn
        l  = length ps
    in if l < 2
         then Nothing
         else case readVersion (last ps) of
                Nothing -> Nothing
                Just v  -> Just (intercalate "-" (take (l-1) ps), v)

------------------------------------------------------------------------------
