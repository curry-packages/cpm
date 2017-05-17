--------------------------------------------------------------------------------
--- This module contains functions for accessing and modifying the global 
--- package cache.
--------------------------------------------------------------------------------

module CPM.PackageCache.Global 
  ( GlobalCache
  , findAllVersions
  , findNewestVersion
  , findVersion
  , isPackageInstalled
  , installedPackageDir
  , readInstalledPackagesFromDir
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

import Directory
import Either
import List
import Maybe (isJust)
import FilePath

import CPM.Config (Config, packageInstallDir)
import CPM.ErrorLogger
import CPM.FileUtil ( copyDirectory, inTempDir, recreateDirectory, inDirectory
                    , removeDirectoryComplete, tempDir
                    , checkAndGetDirectoryContents, quote )
import CPM.Package
import CPM.Repository

--- The global package cache.
data GlobalCache = GlobalCache [Package]

--- An empty package cache.
emptyCache :: GlobalCache
emptyCache = GlobalCache []

--- Gets all package specifications from a cache.
allPackages :: GlobalCache -> [Package]
allPackages (GlobalCache ps) = ps

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
findVersion (GlobalCache ps) p v = if length hits == 0
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
installedPackageDir cfg pkg =
  base  </> (name pkg ++ "-" ++ (showVersion $ version pkg))
 where 
  base = packageInstallDir cfg

--- Copy a package version to a directory.
copyPackage :: Config -> Package -> String -> IO (ErrorLogger ())
copyPackage cfg pkg dir = do
  exists <- doesDirectoryExist srcDir
  if not exists
    then failIO $ "Package '" ++ packageId pkg ++ "' not installed"
    else copyDirectory srcDir dir >> succeedIO ()
 where
  srcDir = installedPackageDir cfg pkg

--- Acquires a package from the source specified in its specification and 
--- installs it to the global package cache.
acquireAndInstallPackage :: Config -> Package -> IO (ErrorLogger ())
acquireAndInstallPackage cfg pkg = case (source pkg) of
  Nothing -> failIO $ "No source specified for " ++ packageId pkg
  Just  s -> log Info ("Installing package from " ++ showPackageSource pkg) |> 
             installFromSource cfg pkg s

--- Installs a package from the given package source to the global package
--- cache.
installFromSource :: Config -> Package -> PackageSource -> IO (ErrorLogger ())
installFromSource cfg pkg (Git url rev) = do
  pkgDirExists <- doesDirectoryExist pkgDir
  if pkgDirExists
    then
      log Info $ "Package '" ++ packageId pkg ++ "' already installed, skipping"
    else do
      c <- inDirectory (packageInstallDir cfg) $ execQuietCmd cloneCommand
      if c == 0
        then case rev of
          Nothing           -> checkoutGitRef pkgDir "HEAD"
          Just (Tag tag)    -> checkoutGitRef pkgDir
                                              (replaceVersionInTag pkg tag)
          Just (Ref ref)    -> checkoutGitRef pkgDir ref
          Just VersionAsTag ->
            let tag = "v" ++ (showVersion $ version pkg) 
            in checkoutGitRef pkgDir tag |> 
               log Info ("Package '" ++ packageId pkg ++ "' installed")
        else removeDirectoryComplete pkgDir >>
             failIO ("Failed to clone repository from '" ++ url ++
                     "', return code " ++ show c)
 where
  pkgDir = packageInstallDir cfg </> packageId pkg
  cloneCommand q = unwords ["git clone", q, quote url, quote $ packageId pkg]

installFromSource cfg pkg (FileSource zip) = do
  absZip <- getAbsolutePath zip
  pkgDirExists <- doesDirectoryExist pkgDir
  if pkgDirExists
    then
      log Info $ "Package '" ++ packageId pkg ++ "' already installed, skipping"
    else do 
      createDirectory pkgDir
      c <- inTempDir $ showExecCmd $ "unzip -qq -d " ++ quote pkgDir ++
                                     " " ++ quote absZip
      if c == 0
        then log Info $ "Installed " ++ (packageId pkg)
        else removeDirectoryComplete pkgDir >>
             failIO ("Failed to unzip package, return code " ++ show c)
 where
  pkgDir = (packageInstallDir cfg) </> (packageId pkg)

installFromSource cfg pkg (Http url) = do
  c <- inTempDir $ showExecCmd $ "curl -s -o package.zip " ++ quote url
  if c == 0
    then do
      pkgDirExists <- doesDirectoryExist pkgDir
      if pkgDirExists
        then log Info $ "Package '" ++ packageId pkg ++
                        "' already installed, skipping"
        else do
          createDirectory pkgDir
          c' <- inTempDir $ showExecCmd $ "unzip -qq -d " ++ quote pkgDir ++
                                          " package.zip"
          if c' == 0
            then do
              c'' <- inTempDir $ showExecCmd ("rm package.zip")
              if c'' == 0
                then log Info ("Installed " ++ packageId pkg)
                else failIO $ "failed to delete package.zip"
            else failIO $ "failed to unzip package.zip " ++ (show c')
    else failIO $ "curl failed with " ++ (show c)
 where
  pkgDir = (packageInstallDir cfg) </> (packageId pkg)

--- Installs a package from a ZIP file to the global package cache.
installFromZip :: Config -> String -> IO (ErrorLogger ())
installFromZip cfg zip = do
  t <- tempDir
  recreateDirectory (t </> "installtmp")
  absZip <- getAbsolutePath zip
  c <- inTempDir $ showExecCmd $ "unzip -qq -d installtmp " ++ quote absZip
  if c == 0
    then do
      loadPackageSpec (t </> "installtmp") |>=
        \pkgSpec -> log Debug ("ZIP contains " ++ (packageId pkgSpec)) |> 
        installFromSource cfg pkgSpec (FileSource zip)
    else failIO "failed to extract ZIP file"

--- Checks out a specific ref of a Git repository
--- 
--- @param dir - the directory containing the repo
--- @param ref - the ref to check out
checkoutGitRef :: String -> String -> IO (ErrorLogger ())
checkoutGitRef dir ref = do
  c <- inDirectory dir $ execQuietCmd (\q -> unwords ["git checkout", q, ref])
  if c == 0
    then succeedIO ()
    else removeDirectoryComplete dir >>
         failIO ("Failed to check out " ++ ref ++ ", return code " ++ show c)

--- Installs a package's missing dependencies.
installMissingDependencies :: Config -> GlobalCache -> [Package] 
                           -> IO (ErrorLogger ())
installMissingDependencies cfg gc deps = if length missing > 0
  then log Info logMsg |>
    mapEL (acquireAndInstallPackage cfg) missing |>
    succeedIO ()
  else succeedIO ()
 where
   missing = filter (not . isPackageInstalled gc) deps
   logMsg = "Installing missing dependencies " ++
            intercalate "," (map packageId missing)

--- Filters a list of packages to the ones not installed in the global package
--- cache.
missingPackages :: GlobalCache -> [Package] -> [Package]
missingPackages gc = filter (not . isPackageInstalled gc)

--- Checkout a package from the global package cache.
checkoutPackage :: Config -> Repository -> GlobalCache -> Package
                -> IO (ErrorLogger ())
checkoutPackage cfg _ _ pkg = do
  sexists <- doesDirectoryExist pkgDir
  texists <- doesDirectoryExist codir
  if texists
    then log Error $ "Local package directory '" ++ codir ++ "' already exists."
    else if sexists
           then copyDirectory pkgDir codir >> log Info logMsg
           else log Error $ "Package '" ++ pkgId ++ "' is not installed."
 where
  pkgId  = packageId pkg
  pkgDir = packageInstallDir cfg </> pkgId
  codir  = name pkg
  logMsg = "Package '" ++ pkgId ++ "' checked out into directory '" ++
           codir ++ "'."

--- Removes a package from the global package cache.
uninstallPackage :: Config -> Repository -> GlobalCache -> String -> Version 
                 -> IO (ErrorLogger ())
uninstallPackage cfg _ _ pkg ver = do
  exists <- doesDirectoryExist pkgDir
  if exists
    then showExecCmd ("rm -Rf " ++ quote pkgDir) >> log Info logMsg
    else log Info $ "Package '" ++ pkgId ++ "' is not installed."
 where
  pkgDir = (packageInstallDir cfg) </> pkgId
  pkgId = pkg ++ "-" ++ showVersion ver
  logMsg = "Package '" ++ pkgId ++ "' uninstalled."

--- Tries to find a package in the global package cache.
tryFindPackage :: GlobalCache -> String -> Version -> IO (ErrorLogger Package)
tryFindPackage gc name ver = case findVersion gc name ver of
  Just pkg -> succeedIO pkg
  Nothing -> failIO $ "Package " ++ name ++ "-" ++ showVersion ver ++
                      " could not be found."

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
  pkgDirs <- checkAndGetDirectoryContents path
  pkgPaths <- return $ filter (not . isPrefixOf ".") pkgDirs
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
      Just p  -> do debugMessage $ "Package spec '" ++ packageId p ++
                                   "' loaded from repository"
                    return (Right p)

  readPackageSpecFromFile pkgdir = do
    let f = path </> pkgdir </> "package.json"
    debugMessage $ "Reading package spec from '" ++ f ++ "'..."
    spec <- readPackageSpecIO $ readFile f
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
