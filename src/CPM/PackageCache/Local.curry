--------------------------------------------------------------------------------
--- This module implements the local package cache. The local package cache is
--- located in the .cpm/package_cache of the current package. It contains
--- symlinks to all dependencies used by the current package. Package files are
--- copied from the local cache to the runtime cache when they need to be used.
--- The package manager usually creates symlinks to the global package cache.
--- Symlinks to other locations can be used to include modified versions of
--- packages that are not yet published to the package repository or installed
--- in the global cache.
--------------------------------------------------------------------------------

module CPM.PackageCache.Local
  ( cacheDir
  , createLinkToGlobalCache
  , linkPackages
  , clearCache
  , createLink
  , doesLinkPointToGlobalCache
  , packageDir
  , isPackageInCache
  , allPackages
  ) where

import Debug.Trace
import System.Directory ( createDirectoryIfMissing, copyFile, getAbsolutePath
                        , getDirectoryContents, doesDirectoryExist
                        , doesFileExist )
import System.FilePath  ( (</>) )
import Data.Either      ( rights )
import Data.List        ( isPrefixOf )
import Control.Monad
import IOExts           ( readCompleteFile )

import CPM.Config       ( Config, packageInstallDir )
import CPM.ErrorLogger
import CPM.FileUtil     ( isSymlink, removeSymlink, createSymlink, linkTarget )
import CPM.Package      ( Package, packageId, readPackageSpec )
import CPM.PackageCache.Global ( installedPackageDir )

--- The cache directory of the local package cache.
---
--- @param dir the package directory
cacheDir :: String -> String
cacheDir pkgDir = pkgDir </> ".cpm" </> "package_cache"

--- Reads all packages specifications from the local package cache.
---
--- @param dir the package directory
allPackages :: String -> IO (ErrorLogger [Package])
allPackages pkgDir = do
  cacheExists <- doesDirectoryExist cdir
  if cacheExists
    then do
      debugMessage $ "Reading local package cache from '" ++ cdir ++ "'..."
      cdircont <- getDirectoryContents cdir
      let pkgDirs = filter (not . isPrefixOf ".") cdircont
      pkgPaths <- mapM removeIfIllegalSymLink $ map (cdir </>) pkgDirs
      specPaths <- return $ map (</> "package.json") $ concat pkgPaths
      specs <- mapM (readPackageSpecIO . readCompleteFile) specPaths
      succeedIO $ rights specs
    else succeedIO []
 where
  readPackageSpecIO = fmap readPackageSpec
  cdir = cacheDir pkgDir

  removeIfIllegalSymLink target = do
    dirExists  <- doesDirectoryExist target
    fileExists <- doesFileExist target
    isLink     <- isSymlink target
    if isLink && (dirExists || fileExists)
      then return [target]
      else when isLink (removeSymlink target >> return ()) >> return []

--- Creates a link to a package from the global cache in the local cache. Does
--- not overwrite existing links.
---
--- @param cfg the current configuration
--- @param dir the package directory
--- @param gc the global package cache
--- @param pkg the package to copy
createLinkToGlobalCache :: Config -> String -> Package -> IO (ErrorLogger ())
createLinkToGlobalCache cfg pkgDir pkg =
  createLink pkgDir (installedPackageDir cfg pkg) (packageId pkg) False

--- Links a list of packages from the global cache into the local cache. Does
--- not overwrite existing links.
---
--- @param cfg the current configuration
--- @param dir the package directory
--- @param gc the global package cache
--- @param pkgs the list of packages
linkPackages :: Config -> String -> [Package]
             -> IO (ErrorLogger ())
linkPackages cfg pkgDir pkgs =
  mapEL (createLinkToGlobalCache cfg pkgDir) pkgs |> succeedIO ()

--- Tests whether a link in the local package cache points to a package in the
--- global package cache.
---
--- @param cfg the current configuration
--- @param gc the global package cache
--- @param dir the package directory
--- @param name the name of the link
doesLinkPointToGlobalCache :: Config -> String -> String -> IO Bool
doesLinkPointToGlobalCache cfg pkgDir name = do
  target <- linkTarget link
  return $ isPrefixOf (packageInstallDir cfg) target
 where
  link = (cacheDir pkgDir) </> name

--- Calculates the local package path of the given package
---
--- @param dir the package directory
--- @param pkg the package
packageDir :: String -> Package -> String
packageDir pkgDir pkg = (cacheDir pkgDir) </> (packageId pkg)

--- Checks whether a package is in the local cache.
---
--- @param dir the package directory
--- @param pkg the package
isPackageInCache :: String -> Package -> IO Bool
isPackageInCache pkgDir pkg = do
  dirExists <- doesDirectoryExist packageDir'
  fileExists <- doesFileExist packageDir'
  return $ dirExists || fileExists
 where
  packageDir' = packageDir pkgDir pkg

--- Clear the local package cache.
---
--- @param dir the package directory
clearCache :: String -> IO ()
clearCache pkgDir = do
  cacheExists <- doesDirectoryExist cdir
  if cacheExists
    then do
      pkgDirs <- getDirectoryContents cdir
      mapM deleteIfLink (map (cdir </>) $ filter (not . isDotOrDotDot) pkgDirs)
      return ()
    else return ()
 where
  cdir = cacheDir pkgDir

ensureCacheDir :: String -> IO String
ensureCacheDir pkgDir = do
  createDirectoryIfMissing True (cacheDir pkgDir)
  return (cacheDir pkgDir)

deleteIfLink :: String -> IO (ErrorLogger ())
deleteIfLink target = do
  dirExists  <- doesDirectoryExist target
  fileExists <- doesFileExist target
  isLink     <- isSymlink target
  if dirExists || fileExists
    then
      if isLink
        then removeSymlink target >> succeedIO ()
        else failIO $ "deleteIfLink can only delete links!\n" ++
                      "Unexpected target: " ++ target
    else
      if isLink -- maybe it is a link to some non-existing target
        then removeSymlink target >> succeedIO ()
        else succeedIO ()

linkExists :: String -> IO Bool
linkExists target = do
  dirExists <- doesDirectoryExist target
  fileExists <- doesFileExist target
  if dirExists || fileExists
    then isSymlink target
    else return False

isDotOrDotDot :: String -> Bool
isDotOrDotDot s = case s of
  "."  -> True
  ".." -> True
  _    -> False

--- Create a link from a directory into the local package cache.
---
--- @param pkgDir the package directory
--- @param from the source directory to be linked into the local cache
--- @param name the name of the link in the package directory (should be a
---        package id)
--- @param replace replace existing link?
createLink :: String -> String -> String -> Bool -> IO (ErrorLogger ())
createLink pkgDir from name replace = do
  ensureCacheDir pkgDir
  exists <- linkExists target
  if exists && not replace
    then succeedIO ()
    else deleteIfLink target |> do
      fromabs <- getAbsolutePath from
      rc <- createSymlink fromabs target
      if rc == 0
        then succeedIO ()
        else failIO $ "Failed to create symlink from '" ++ from ++ "' to '" ++
                      target ++ "', return code " ++ show rc
 where
  target = cacheDir pkgDir </> name
