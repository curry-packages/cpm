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

import Control.Applicative (when)
import Data.Either
import Data.List
import Data.Maybe       (isJust)
import System.FilePath
import System.Directory
import System.IOExts    ( readCompleteFile )

import CPM.Config       ( Config, packageInstallDir, packageTarFilesURLs )
import CPM.ErrorLogger
import CPM.FileUtil     ( cleanTempDir, copyDirectory, getRealPath
                        , recreateDirectory, recreateDirectory, inDirectory
                        , removeDirectoryComplete
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
  exists <- liftIOEL $ doesDirectoryExist srcDir
  if not exists
    then fail $ "Package '" ++ packageId pkg ++ "' not installed"
    else liftIOEL (copyDirectory srcDir dir) >> return ()
 where
  srcDir = installedPackageDir cfg pkg

--- Acquires a package, either from the global tar file repository
--- or from the source specified in its specification, and 
--- installs it to the global package cache.
acquireAndInstallPackage :: Config -> Package -> ErrorLogger ()
acquireAndInstallPackage cfg pkg = do
  pkgDirExists <- liftIOEL $ doesDirectoryExist (installedPackageDir cfg pkg)
  if pkgDirExists
    then logInfo $ "Package '" ++ packageId pkg ++
                       "' already installed, skipping"
    else tryInstallFromURLs (packageTarFilesURLs cfg)
 where
  tryInstallFromURLs []         = fail "No URLs for installations"
  tryInstallFromURLs (url:urls) = do
    let stdurl = url ++ "/" ++ packageId pkg ++ ".tar.gz"
    logInfo $ "Installing package from " ++ stdurl
    ll  <- getLogLevel
    err <- tryEL (ll /= Debug) $
             installPackageSourceTo pkg (Http stdurl) (packageInstallDir cfg)
    case err of
      Left  _ -> if null urls
                   then do -- Try to download the source repo of the package:
                           pkgspec <- readPackageFromRepository cfg pkg
                           maybe (fail downloadError)
                                 (installFromSource cfg pkgspec)
                                 (source pkgspec)
                   else tryInstallFromURLs urls
      Right _ -> acquireAndInstallPackageFromSource cfg pkg

  downloadError =
    "Package downloading failed. Use option '-v debug' for more infos."

--- Acquires a package from the source specified in its specification and 
--- installs it to the global package cache.
acquireAndInstallPackageFromSource :: Config -> Package -> ErrorLogger ()
acquireAndInstallPackageFromSource cfg reppkg = do
  pkg <- readPackageFromRepository cfg reppkg
  case source pkg of
    Nothing -> fail $ "No source specified for " ++ packageId pkg
    Just  s -> do logInfo $ "Installing package '" ++ packageId pkg ++ "'..."
                  installFromSource cfg pkg s

------------------------------------------------------------------------------
--- Installs a package from the given package source to the global package
--- cache.
installFromSource :: Config -> Package -> PackageSource -> ErrorLogger ()
installFromSource cfg pkg pkgsource = do
  pkgDirExists <- liftIOEL $ doesDirectoryExist pkgDir
  if pkgDirExists
    then
      logInfo $ "Package '" ++ packageId pkg ++ "' already installed, skipping"
    else
      logInfo ("Installing package from " ++ showSourceOfPackage pkg) >> 
      installPackageSourceTo pkg pkgsource (packageInstallDir cfg)
 where
  pkgDir = installedPackageDir cfg pkg

--- Installs a package from a ZIP file to the global package cache.
installFromZip :: Config -> String -> ErrorLogger ()
installFromZip cfg zip = do
  t <- liftIOEL tempDir
  liftIOEL $ recreateDirectory (t </> "installtmp")
  absZip <- liftIOEL $ getRealPath zip
  c <- inTempDirEL $ showExecCmd $ "unzip -qq -d installtmp " ++ quote absZip
  if c == 0
    then do
      pkgSpec <- loadPackageSpec (t </> "installtmp")
      logDebug ("ZIP contains " ++ packageId pkgSpec)
      liftIOEL cleanTempDir
      installFromSource cfg pkgSpec (FileSource zip)
    else do
      liftIOEL cleanTempDir
      fail "failed to extract ZIP file"

--- Installs a package's missing dependencies.
installMissingDependencies :: Config -> GlobalCache -> [Package] 
                           -> ErrorLogger ()
installMissingDependencies cfg gc deps = when (length missing > 0) $ do
  logInfo logmsg
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
checkoutPackage :: Config -> Package -> String -> ErrorLogger ()
checkoutPackage cfg pkg outdir = do
  sexists <- liftIOEL $ doesDirectoryExist pkgDir
  let codir = if null outdir then name pkg else outdir
  texists <- liftIOEL $ doesDirectoryExist codir
  if texists
    then logError $
           "Local package directory '" ++ codir ++ "' already exists."
    else if sexists
           then do liftIOEL $ copyDirectory pkgDir codir
                   logInfo (logmsg codir)
           else logError $ "Package '" ++ pkgId ++ "' is not installed."
 where
  pkgId    = packageId pkg
  pkgDir   = installedPackageDir cfg pkg
  logmsg d = "Package '" ++ pkgId ++ "' checked out into directory '" ++
             d ++ "'."

--- Removes a package from the global package cache.
uninstallPackage :: Config -> String -> Version -> ErrorLogger ()
uninstallPackage cfg pkgname ver = do
  let pkgId  = pkgname ++ "-" ++ showVersion ver
      pkgDir = packageInstallDir cfg </> pkgId
  exists <- liftIOEL $ doesDirectoryExist pkgDir
  if exists
    then do showExecCmd ("rm -Rf " ++ quote pkgDir)
            logInfo $ "Package '" ++ pkgId ++ "' uninstalled."
    else logInfo $ "Package '" ++ pkgId ++ "' is not installed."

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
  logDebug $ "Reading global package cache from '" ++ path ++ "'..."
  pkgPaths <- liftIOEL $ checkAndGetVisibleDirectoryContents path
  specs <- mapM loadPackageSpecFromDir pkgPaths
  if null (lefts specs)
    then do logDebug "Finished reading global package cache"
            return (Right $ GlobalCache (rights specs))
    else return (Left $ intercalate "; " (lefts specs))
 where
  readPackageSpecIO = liftIOEL . fmap readPackageSpec

  loadPackageSpecFromDir pkgdir = case packageVersionFromFile pkgdir of
    Nothing -> readPackageSpecFromFile pkgdir
    Just (pn,pv) -> case CPM.Repository.findVersion repo pn pv of
      Nothing -> readPackageSpecFromFile pkgdir
      Just p  -> return (Right p)

  readPackageSpecFromFile pkgdir = do
    let f = path </> pkgdir </> packageSpecFile
    logDebug $ "Reading package spec from '" ++ f ++ "'..."
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
