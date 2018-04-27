--------------------------------------------------------------------------------
--- Contains functions that operate on a package copy. And some functions that
--- don't quite fit anywhere else.
--------------------------------------------------------------------------------

module CPM.PackageCopy
  ( resolveDependenciesForPackageCopy
  , resolveAndCopyDependencies, resolveAndCopyDependenciesForPackage
  , resolveDependencies
  , upgradeAllPackages
  , upgradeSinglePackage
  , linkToLocalCache
  , acquireAndInstallPackageWithDependencies
  , installLocalDependencies
  ) where

import Directory ( doesDirectoryExist )
import List      ( intercalate )
import Maybe     ( mapMaybe)

import CPM.Config     ( Config )
import CPM.Repository ( Repository, allPackages )
import qualified CPM.LookupSet as LS
import CPM.ErrorLogger
import qualified CPM.PackageCache.Global as GC
import qualified CPM.PackageCache.Runtime as RuntimeCache
import qualified CPM.PackageCache.Local as LocalCache
import CPM.Package
import CPM.Resolution

--- Resolves dependencies for a package copy.
resolveDependenciesForPackageCopy :: Config -> Package -> Repository 
                                  -> GC.GlobalCache -> String 
                                  -> IO (ErrorLogger ResolutionResult)
resolveDependenciesForPackageCopy cfg pkg repo gc dir = 
  lookupSetForPackageCopy cfg pkg repo gc dir |>= \lookupSet ->
  resolveDependenciesFromLookupSet cfg pkg lookupSet

--- Calculates the lookup set needed for dependency resolution on a package
--- copy.
lookupSetForPackageCopy :: Config -> Package -> Repository -> GC.GlobalCache 
                        -> String -> IO (ErrorLogger LS.LookupSet)
lookupSetForPackageCopy cfg _ repo gc dir =
  LocalCache.allPackages dir |>=
  \localPkgs -> do
    diffInLC <- mapIO filterGCLinked localPkgs
    let lsLC = LS.addPackages lsGC localPkgs LS.FromLocalCache in
      mapEL logSymlinkedPackage (mapMaybe id diffInLC) |>
      succeedIO lsLC
 where
  logSymlinkedPackage p = log Debug $ "Using symlinked version of '" ++ (packageId p) ++ "' from local cache."
  lsRepo = LS.addPackages LS.emptySet (allPackages repo) LS.FromRepository
  -- Find all packages that are in the global cache, but not in the repo
  newInGC = filter (\p -> not $ any (packageIdEq p) (allPackages repo)) $ GC.allPackages gc
  lsGC = LS.addPackages lsRepo newInGC LS.FromGlobalCache
  filterGCLinked p = do
    points <- LocalCache.doesLinkPointToGlobalCache cfg gc dir (packageId p)
    return $ if points
      then Nothing
      else Just p

--- Resolves dependencies for a package.
resolveDependenciesForPackage :: Config -> Package -> Repository
                              -> GC.GlobalCache 
                              -> IO (ErrorLogger ResolutionResult)
resolveDependenciesForPackage cfg pkg repo gc = 
  resolveDependenciesFromLookupSet cfg pkg lookupSet
 where
  lsRepo = LS.addPackages LS.emptySet (allPackages repo) LS.FromRepository
  -- Find all packages that are in the global cache, but not in the repo
  newInGC = filter inGCButNotInRepo $ GC.allPackages gc
  inGCButNotInRepo p = not $ any (packageIdEq p) (allPackages repo)
  lookupSet = LS.addPackages lsRepo newInGC LS.FromGlobalCache

--- Acquires a package and its dependencies and installs them to the global
--- package cache.
acquireAndInstallPackageWithDependencies :: Config -> Repository 
                                         -> GC.GlobalCache -> Package 
                                         -> IO (ErrorLogger ())
acquireAndInstallPackageWithDependencies cfg repo gc pkg = 
  resolveDependenciesForPackage cfg pkg repo gc |>=
  \result -> GC.installMissingDependencies cfg gc (resolvedPackages result) |>
  GC.acquireAndInstallPackage cfg pkg

--- Links the dependencies of a package to its local cache and copies them to
--- its runtime cache.
copyDependencies :: Config -> GC.GlobalCache -> Package -> [Package] -> String 
                 -> IO (ErrorLogger ())
copyDependencies cfg gc pkg pkgs dir = 
  LocalCache.linkPackages cfg dir gc pkgs |>
  RuntimeCache.copyPackages cfg pkgs' dir >> succeedIO ()
 where 
  pkgs' = filter (/= pkg) pkgs

--- Upgrades all dependencies of a package copy.
upgradeAllPackages :: Config -> Repository -> GC.GlobalCache -> String 
                   -> IO (ErrorLogger ())
upgradeAllPackages cfg repo gc dir = loadPackageSpec dir |>=
  \pkgSpec -> LocalCache.clearCache dir >> succeedIO () |>
  installLocalDependencies cfg repo gc dir |>=
  \ (_,deps) -> copyDependencies cfg gc pkgSpec deps dir

--- Upgrades a single dependencies and its transitive dependencies.
upgradeSinglePackage :: Config -> Repository -> GC.GlobalCache -> String 
                     -> String -> IO (ErrorLogger ())
upgradeSinglePackage cfg repo gc dir pkgName = loadPackageSpec dir |>=
  \pkgSpec -> lookupSetForPackageCopy cfg pkgSpec repo gc dir |>=
  \originalLS -> let transitiveDeps = pkgName : allTransitiveDependencies originalLS pkgName in
  resolveDependenciesFromLookupSet cfg pkgSpec
                        (LS.setLocallyIgnored originalLS transitiveDeps) |>=
  \result -> GC.installMissingDependencies cfg gc (resolvedPackages result) |>
  log Info (showDependencies result) |>
  copyDependencies cfg gc pkgSpec (resolvedPackages result) dir

--- Installs the dependencies of a package.
installLocalDependencies :: Config -> Repository -> GC.GlobalCache -> String 
                         -> IO (ErrorLogger (Package,[Package]))
installLocalDependencies cfg repo gc dir =
  loadPackageSpec dir |>= \pkgSpec ->
  resolveDependenciesForPackageCopy cfg pkgSpec repo gc dir |>= \result ->
  GC.installMissingDependencies cfg gc (resolvedPackages result) |>
  log Info (showDependencies result) |> 
  copyDependencies cfg gc pkgSpec (resolvedPackages result) dir |>
  succeedIO (pkgSpec, resolvedPackages result)

--- Links a directory into the local package cache. Used for `cypm link`.
linkToLocalCache :: String -> String -> IO (ErrorLogger ())
linkToLocalCache src pkgDir = do
  dirExists <- doesDirectoryExist src
  if dirExists
    then loadPackageSpec src |>= \pkgSpec ->
         LocalCache.createLink pkgDir src (packageId pkgSpec) True |> 
         succeedIO ()
    else log Critical ("Directory '" ++ src ++ "' does not exist.") |>
         succeedIO ()

--- Resolves the dependencies for a package copy and fills the package caches.
resolveAndCopyDependencies :: Config -> Repository -> GC.GlobalCache -> String 
                           -> IO (ErrorLogger [Package])
resolveAndCopyDependencies cfg repo gc dir =
  loadPackageSpec dir |>= resolveAndCopyDependenciesForPackage cfg repo gc dir

--- Resolves the dependencies for a package copy and fills the package caches.
resolveAndCopyDependenciesForPackage ::
     Config -> Repository -> GC.GlobalCache -> String -> Package
  -> IO (ErrorLogger [Package])
resolveAndCopyDependenciesForPackage cfg repo gc dir pkgSpec =
  resolveDependenciesForPackageCopy cfg pkgSpec repo gc dir |>= \result -> 
    let deps = resolvedPackages result
        missingDeps = GC.missingPackages gc deps 
        failMsg = "Missing dependencies " 
                  ++ (intercalate "," $ map packageId missingDeps) 
                  ++ "\nUse `cypm install` to install missing dependencies."
    in if length missingDeps > 0
         then failIO failMsg
         else copyDependencies cfg gc pkgSpec deps dir |>= \_ ->
              succeedIO deps

--- Resolves the dependencies for a package copy.
resolveDependencies :: Config -> Repository -> GC.GlobalCache -> String 
                    -> IO (ErrorLogger ResolutionResult)
resolveDependencies cfg repo gc dir = loadPackageSpec dir |->
  log Info ("Read package spec from " ++ dir) |>=
  \pkgSpec -> resolveDependenciesForPackageCopy cfg pkgSpec repo gc dir

------------------------------------------------------------------------------
