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
  , acquireAndInstallPackageFromSource
  , tryFindPackage
  , missingPackages
  , installFromZip
  , checkoutPackage
  , uninstallPackage
  , emptyCache
  ) where

import Directory
import Either
import IOExts       ( readCompleteFile )
import List
import Maybe (isJust)
import FilePath

import CPM.Config   ( Config, packageInstallDir, packageTarFilesURL )
import CPM.ErrorLogger
import CPM.FileUtil ( cleanTempDir, copyDirectory, inTempDir
                    , recreateDirectory, inDirectory
                    , removeDirectoryComplete, tempDir, whenFileExists
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
copyPackage :: Config -> Package -> String -> IO (ErrorLogger ())
copyPackage cfg pkg dir = do
  exists <- doesDirectoryExist srcDir
  if not exists
    then failIO $ "Package '" ++ packageId pkg ++ "' not installed"
    else copyDirectory srcDir dir >> succeedIO ()
 where
  srcDir = installedPackageDir cfg pkg

--- Acquires a package, either from the global tar file repository
--- or from the source specified in its specification, and 
--- installs it to the global package cache.
acquireAndInstallPackage :: Config -> Package -> ErrorLoggerIO ()
acquireAndInstallPackage cfg pkg = do
  pkgDirExists <- execIO $ doesDirectoryExist (installedPackageDir cfg pkg)
  if pkgDirExists
    then logMsg Info $ "Package '" ++ packageId pkg ++
                       "' already installed, skipping"
    else do
      let stdurl = packageTarFilesURL cfg ++ packageId pkg ++ ".tar.gz"
      logMsg Info ("Installing package from " ++ stdurl)
      (msgs,err) <- execIO $ installPackageSourceTo pkg (Http stdurl)
                                                    (packageInstallDir cfg)
      case err of
        Right _ -> toELM $ return (msgs,err)
        Left  _ -> toELM $ acquireAndInstallPackageFromSource cfg pkg

--- Acquires a package from the source specified in its specification and 
--- installs it to the global package cache.
acquireAndInstallPackageFromSource :: Config -> Package -> IO (ErrorLogger ())
acquireAndInstallPackageFromSource cfg reppkg =
  readPackageFromRepository cfg reppkg |>= \pkg ->
  case source pkg of
   Nothing -> failIO $ "No source specified for " ++ packageId pkg
   Just  s -> log Info ("Installing package '" ++ packageId pkg ++ "'...") |> 
              installFromSource cfg pkg s

------------------------------------------------------------------------------
--- Installs a package from the given package source to the global package
--- cache.
installFromSource :: Config -> Package -> PackageSource -> IO (ErrorLogger ())
installFromSource cfg pkg pkgsource = do
  pkgDirExists <- doesDirectoryExist pkgDir
  if pkgDirExists
    then
      log Info $ "Package '" ++ packageId pkg ++ "' already installed, skipping"
    else
      log Info ("Installing package from " ++ showSourceOfPackage pkg) |> 
      installPackageSourceTo pkg pkgsource (packageInstallDir cfg)
 where
  pkgDir = installedPackageDir cfg pkg

--- Installs a package from a ZIP file to the global package cache.
installFromZip :: Config -> String -> IO (ErrorLogger ())
installFromZip cfg zip = do
  t <- tempDir
  recreateDirectory (t </> "installtmp")
  absZip <- getAbsolutePath zip
  c <- inTempDir $ showExecCmd $ "unzip -qq -d installtmp " ++ quote absZip
  if c == 0
    then
      loadPackageSpec (t </> "installtmp") |>= \pkgSpec ->
      log Debug ("ZIP contains " ++ packageId pkgSpec) |> 
      (cleanTempDir >> succeedIO ()) |>
      installFromSource cfg pkgSpec (FileSource zip)
    else cleanTempDir >> failIO "failed to extract ZIP file"

--- Installs a package's missing dependencies.
installMissingDependencies :: Config -> GlobalCache -> [Package] 
                           -> ErrorLoggerIO ()
installMissingDependencies cfg gc deps = whenM (length missing > 0) $ do
  logMsg Info logmsg
  mapM_ (acquireAndInstallPackage cfg) missing
 where
   missing = filter (not . isPackageInstalled gc) deps
   logmsg = "Installing missing dependencies " ++
            intercalate "," (map packageId missing)

--- Filters a list of packages to the ones not installed in the global package
--- cache.
missingPackages :: GlobalCache -> [Package] -> [Package]
missingPackages gc = filter (not . isPackageInstalled gc)

--- Checkout a package from the global package cache.
checkoutPackage :: Config -> Package -> ErrorLoggerIO ()
checkoutPackage cfg pkg = do
  sexists <- execIO $ doesDirectoryExist pkgDir
  texists <- execIO $ doesDirectoryExist codir
  if texists
    then logMsg Error $
           "Local package directory '" ++ codir ++ "' already exists."
    else if sexists
           then do execIO $ copyDirectory pkgDir codir
                   logMsg Info logmsg
           else logMsg Error $ "Package '" ++ pkgId ++ "' is not installed."
 where
  pkgId  = packageId pkg
  pkgDir = installedPackageDir cfg pkg
  codir  = name pkg
  logmsg = "Package '" ++ pkgId ++ "' checked out into directory '" ++
           codir ++ "'."

--- Removes a package from the global package cache.
uninstallPackage :: Config -> String -> Version -> ErrorLoggerIO ()
uninstallPackage cfg pkgname ver = do
  let pkgId  = pkgname ++ "-" ++ showVersion ver
      pkgDir = packageInstallDir cfg </> pkgId
  exists <- execIO $ doesDirectoryExist pkgDir
  if exists
    then do execIO $ showExecCmd ("rm -Rf " ++ quote pkgDir)
            logMsg Info $ "Package '" ++ pkgId ++ "' uninstalled."
    else logMsg Info $ "Package '" ++ pkgId ++ "' is not installed."

--- Tries to find a package in the global package cache.
tryFindPackage :: GlobalCache -> String -> Version -> IO (ErrorLogger Package)
tryFindPackage gc name ver = case findVersion gc name ver of
  Just pkg -> succeedIO pkg
  Nothing -> failIO $ "Package " ++ name ++ "-" ++ showVersion ver ++
                      " could not be found."

--- Reads the global package cache.
readGlobalCache :: Config -> Repository -> ErrorLoggerIO GlobalCache
readGlobalCache config repo = do
  maybeGC <- execIO $
               readInstalledPackagesFromDir repo $ packageInstallDir config
  case maybeGC of
    Left err -> failELM $ "Error reading global package cache: " ++ err
    Right gc -> return gc

--- Tries to read package specifications from a GC directory structure.
--- If some GC package directory has the same name as a package from
--- the repository index, the package specification from the repository
--- is used, otherwise (this case should not occur) the package specification
--- stored in the directory is read.
--- This should result in faster GC loading.
readInstalledPackagesFromDir :: Repository -> String
                             -> IO (Either String GlobalCache)
readInstalledPackagesFromDir repo path = do
  debugMessage $ "Reading global package cache from '" ++ path ++ "'..."
  pkgPaths <- checkAndGetVisibleDirectoryContents path
  specs <- mapIO loadPackageSpecFromDir pkgPaths
  if null (lefts specs)
    then do debugMessage "Finished reading global package cache"
            return (Right $ GlobalCache (rights specs))
    else return (Left $ intercalate "; " (lefts specs))
 where
  readPackageSpecIO = liftIO readPackageSpec

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
