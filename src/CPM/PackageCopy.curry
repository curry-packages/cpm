--------------------------------------------------------------------------------
--- This module contains operations that operate on a package copy.
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

import System.Directory ( doesDirectoryExist )
import Data.List        ( intercalate )
import Data.Maybe       ( mapMaybe )
import Prelude hiding   ( log )

import CPM.Config       ( Config, baseVersion )
import CPM.Repository   ( Repository, allPackages )
import CPM.Repository.Select
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
                                  -> ErrorLogger ResolutionResult
resolveDependenciesForPackageCopy cfg pkg repo gc dir =
  lookupSetForPackageCopy cfg pkg repo gc dir >>= \lookupSet ->
  resolveDependenciesFromLookupSet cfg (setBaseDependency cfg pkg) lookupSet

--- Calculates the lookup set needed for dependency resolution on a package
--- copy.
lookupSetForPackageCopy :: Config -> Package -> Repository -> GC.GlobalCache
                        -> String -> ErrorLogger LS.LookupSet
lookupSetForPackageCopy cfg _ repo gc dir = do
  localPkgs <- LocalCache.allPackages dir
  diffInLC <- liftIOErrorLogger $ mapM filterGCLinked localPkgs
  let lsLC = addPackagesWOBase cfg lsGC localPkgs LS.FromLocalCache
  mapM logSymlinkedPackage (mapMaybe id diffInLC)
  return lsLC
 where
  allRepoPackages = allPackages repo
  logSymlinkedPackage p = log Debug $ "Using symlinked version of '" ++
                                      packageId p ++ "' from local cache."
  lsRepo = addPackagesWOBase cfg LS.emptySet allRepoPackages LS.FromRepository
  -- Find all packages that are in the global cache, but not in the repo
  newInGC = filter (\p -> not $ any (packageIdEq p) allRepoPackages)
                   (GC.allPackages gc)
  lsGC = addPackagesWOBase cfg lsRepo newInGC LS.FromGlobalCache
  filterGCLinked p = do
    points <- LocalCache.doesLinkPointToGlobalCache cfg dir (packageId p)
    return $ if points
      then Nothing
      else Just p

--- Resolves dependencies for a package.
resolveDependenciesForPackage :: Config -> Package -> Repository
                              -> GC.GlobalCache
                              -> ErrorLogger ResolutionResult
resolveDependenciesForPackage cfg pkg repo gc =
  resolveDependenciesFromLookupSet cfg (setBaseDependency cfg pkg) lookupSet
 where
  lsRepo = addPackagesWOBase cfg LS.emptySet (allPackages repo)
                             LS.FromRepository
  -- Find all packages that are in the global cache, but not in the repo
  newInGC = filter inGCButNotInRepo $ GC.allPackages gc
  inGCButNotInRepo p = not $ any (packageIdEq p) (allPackages repo)
  lookupSet = addPackagesWOBase cfg lsRepo newInGC LS.FromGlobalCache

--- Acquires a package and its dependencies and installs them to the global
--- package cache.
acquireAndInstallPackageWithDependencies :: Config -> Repository -> Package
                                         -> ErrorLogger ()
acquireAndInstallPackageWithDependencies cfg repo pkg =
  GC.readGlobalCache cfg repo >>= \gc ->
  resolveDependenciesForPackage cfg pkg repo gc >>= \result ->
  GC.installMissingDependencies cfg gc (resolvedPackages result) >>
  GC.acquireAndInstallPackage cfg pkg

--- Links the dependencies of a package to its local cache and copies them to
--- its runtime cache. Returns the package specifications of the dependencies.
copyDependencies :: Config -> Package -> [Package] -> String
                 -> ErrorLogger [Package]
copyDependencies cfg pkg pkgs dir =
  LocalCache.linkPackages cfg dir pkgs >>
  RuntimeCache.copyPackages cfg pkgs' dir >>= \pkgspecs ->
  return (if pkg `elem` pkgs then pkg : pkgspecs else pkgspecs)
 where
  pkgs' = filter (/= pkg) pkgs

--- Upgrades all dependencies of a package copy.
upgradeAllPackages :: Config -> String -> ErrorLogger ()
upgradeAllPackages cfg dir =
  loadPackageSpec dir >>= \pkgSpec ->
  LocalCache.clearCache dir >> return () >>
  installLocalDependencies cfg dir >>= \ (_,deps) ->
  copyDependencies cfg pkgSpec deps dir >> return ()

--- Upgrades a single package and its transitive dependencies.
upgradeSinglePackage :: Config -> String -> String -> ErrorLogger ()
upgradeSinglePackage cfg dir pkgName =
  loadPackageSpec dir >>= \pkgSpec ->
  getRepoForPackageSpec cfg pkgSpec >>= \repo ->
  GC.readGlobalCache cfg repo >>= \gc ->
  lookupSetForPackageCopy cfg pkgSpec repo gc dir >>= \originalLS ->
  let transitiveDeps = pkgName : allTransitiveDependencies originalLS pkgName in
  resolveDependenciesFromLookupSet cfg (setBaseDependency cfg pkgSpec)
                        (LS.setLocallyIgnored originalLS transitiveDeps) >>=
  \result -> GC.installMissingDependencies cfg gc (resolvedPackages result) >>
  log Info (showDependencies result) >>
  copyDependencies cfg pkgSpec (resolvedPackages result) dir >> return ()

--- Installs the dependencies of a package.
installLocalDependencies :: Config -> String
                         -> (ErrorLogger (Package,[Package]))
installLocalDependencies cfg dir =
  loadPackageSpec dir >>= \pkgSpec ->
  getRepoForPackageSpec cfg pkgSpec >>= \repo ->
  installLocalDependenciesWithRepo cfg repo dir pkgSpec

--- Installs the dependencies of a package.
installLocalDependenciesWithRepo :: Config -> Repository -> String -> Package
                                 -> ErrorLogger (Package,[Package])
installLocalDependenciesWithRepo cfg repo dir pkgSpec =
  GC.readGlobalCache cfg repo >>= \gc ->
  resolveDependenciesForPackageCopy cfg pkgSpec repo gc dir >>= \result ->
  GC.installMissingDependencies cfg gc (resolvedPackages result) >>
  log Info (showDependencies result) >>
  copyDependencies cfg pkgSpec (resolvedPackages result) dir >>= \cpkgs ->
  return (pkgSpec, cpkgs)

--- Links a directory into the local package cache. Used for `cypm link`.
linkToLocalCache :: Config -> String -> String -> ErrorLogger ()
linkToLocalCache cfg src pkgDir = do
  dirExists <- liftIOErrorLogger $ doesDirectoryExist src
  if dirExists
    then loadPackageSpec src >>= \pkgSpec ->
         getPackageVersion cfg (name pkgSpec) (version pkgSpec) >>=
         maybe
           (log Critical
                ("Package '" ++ packageId pkgSpec ++ "' not in repository!\n" ++
                 "Note: you can only link copies of existing packages."))
           (\_ -> LocalCache.createLink pkgDir src (packageId pkgSpec) True >>
                  return ())
    else log Critical ("Directory '" ++ src ++ "' does not exist.")

--- Resolves the dependencies for a package copy and fills the package caches.
resolveAndCopyDependencies :: Config -> Repository -> GC.GlobalCache -> String
                           -> ErrorLogger [Package]
resolveAndCopyDependencies cfg repo gc dir =
  loadPackageSpec dir >>= resolveAndCopyDependenciesForPackage' cfg repo gc dir

--- Resolves the dependencies for a package copy and fills the package caches.
resolveAndCopyDependenciesForPackage ::
     Config -> String -> Package -> ErrorLogger [Package]
resolveAndCopyDependenciesForPackage cfg dir pkgSpec =
  getRepoForPackageSpec cfg pkgSpec >>= \repo ->
  GC.readGlobalCache cfg repo >>= \gc ->
  resolveAndCopyDependenciesForPackage' cfg repo gc dir pkgSpec

resolveAndCopyDependenciesForPackage' ::
     Config -> Repository -> GC.GlobalCache -> String -> Package
  -> ErrorLogger [Package]
resolveAndCopyDependenciesForPackage' cfg repo gc dir pkgSpec =
  resolveDependenciesForPackageCopy cfg pkgSpec repo gc dir >>= \result ->
    let deps = resolvedPackages result
        missingDeps = GC.missingPackages gc deps
        failMsg = "Missing dependencies "
                  ++ (intercalate "," $ map packageId missingDeps)
                  ++ "\nUse `cypm install` to install missing dependencies."
    in if null missingDeps
         then copyDependencies cfg pkgSpec deps dir
         else fail failMsg

--- Resolves the dependencies for a package copy.
resolveDependencies :: Config -> String -> ErrorLogger ResolutionResult
resolveDependencies cfg dir = do
  pkgSpec <- loadPackageSpec dir
  log Info ("Read package spec from " ++ dir)
  repo <- getRepoForPackageSpec cfg pkgSpec
  gc <- GC.readGlobalCache cfg repo
  resolveDependenciesForPackageCopy cfg pkgSpec repo gc dir

------------------------------------------------------------------------------
--- Sets `base` package dependency in a package to the current `baseVersion`
--- if this dependency is compatible with the current `baseVersion`.
--- Hence, a conflict occurs if some package requires a different version
--- of the `base` package.
setBaseDependency :: Config -> Package -> Package
setBaseDependency cfg pkg =
  pkg { dependencies = map setBase (dependencies pkg) }
 where
  bv = maybe (0,0,0,Nothing) id (readVersion (baseVersion cfg))

  setBase (Dependency n disj) =
    Dependency n $ if n == "base" && isDisjunctionCompatible bv disj
                     then [[VExact bv]]
                     else disj

--- Same as `LS.addPackages` but set the `base` package dependency.
addPackagesWOBase :: Config -> LS.LookupSet -> [Package] -> LS.LookupSource
                  -> LS.LookupSet
addPackagesWOBase cfg ls pkgs src =
  LS.addPackages ls (map (setBaseDependency cfg) pkgs) src

------------------------------------------------------------------------------
