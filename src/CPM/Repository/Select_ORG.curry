------------------------------------------------------------------------------
--- Some queries on the repository cache.
---
--- @author Michael Hanus
--- @version April 2021
------------------------------------------------------------------------------
{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode --optF=-o #-}

module CPM.Repository.Select
  ( searchNameSynopsisModules
  , searchExportedModules, searchExecutable
  , getRepositoryWithNameVersionSynopsis
  , getRepositoryWithNameVersionSynopsisDeps
  , getRepositoryWithNameVersionCategory
  , getBaseRepository
  , getRepoForPackageSpec
  , getRepoForPackages
  , getAllPackageVersions, getPackageVersion
  , addPackageToRepositoryCache
  , updatePackageInRepositoryCache
  )
 where

import Data.Char        ( toLower )
import Data.List        ( isInfixOf )
import Data.Maybe       ( maybeToList )
import System.Directory ( doesFileExist )
import ReadShowTerm

import Database.CDBI.ER
import Database.CDBI.Connection

import CPM.Config      ( Config )
import CPM.ErrorLogger
import CPM.FileUtil    ( ifFileExists )
import CPM.Repository.RepositoryDB
import CPM.Repository.CacheFile ( readRepository )
import CPM.Repository.CacheDB
import CPM.Package
import CPM.Repository

--- Runs a query on the repository cache DB and show debug infos.
runQuery :: Config -> DBAction a -> ErrorLogger a
runQuery cfg dbact = do
  warnIfRepositoryOld cfg
  let dbfile = repositoryCacheDB cfg
  logDebug $ "Reading repository database '" ++ dbfile ++ "'..."
  result <- liftIOEL $ runQueryOnDB dbfile dbact
  logDebug $ "Finished reading repository database"
  return result

--- Returns the packages of the repository containing a given string
--- in the name, synopsis, or exported modules.
--- In each package, the name, version, synopsis, and compilerCompatibility
--- is set.
searchNameSynopsisModules :: Config -> String -> ErrorLogger [Package]
searchNameSynopsisModules cfg pat =
  runQuery cfg $ fmap (map toPackage)
    ``sql* Select Name, Version, Synopsis, CompilerCompatibility
           From   IndexEntry
           Where  Name            like {pattern} Or
                  Synopsis        like {pattern} Or
                  ExportedModules like {pattern};''
 where
  pattern = "%" ++ pat ++ "%"

  toPackage (nm,vs,syn,cmp) =
    emptyPackage { name = nm
                 , version = pkgRead vs
                 , synopsis = syn
                 , compilerCompatibility = pkgRead cmp
                 }

--- Returns the packages of the repository containing a given module
--- in the list of exported modules.
--- In each package, the name, version, synopsis, compilerCompatibility,
--- and exportedModules is set.
searchExportedModules :: Config -> String -> ErrorLogger [Package]
searchExportedModules cfg pat =
  (queryDBorCache cfg True $
     fmap (pkgsToRepository . map toPackage)
       ``sql* Select Name, Version, Synopsis, CompilerCompatibility,
                     ExportedModules
              From   IndexEntry
              Where  ExportedModules like {pattern};''
  ) >>= return . filterExpModules . allPackages
 where
  pattern = "%" ++ pat ++ "%"
  lpat    = map toLower pat

  filterExpModules = filter (\p -> any (\m -> lpat `isInfixOf` (map toLower m))
                                       (exportedModules p))

  toPackage (nm,vs,syn,cmp,exps) =
    emptyPackage { name = nm
                 , version = pkgRead vs
                 , synopsis = syn
                 , compilerCompatibility = pkgRead cmp
                 , exportedModules       = pkgRead exps
                 }

--- Returns the packages of the repository containing a given string
--- in the name of the executable.
--- In each package, the name, version, synopsis, compilerCompatibility,
--- and executableSpec is set.
searchExecutable :: Config -> String -> ErrorLogger [Package]
searchExecutable cfg pat =
  (queryDBorCache cfg True $
     fmap (pkgsToRepository . map toPackage)
       ``sql* Select Name, Version, Synopsis, CompilerCompatibility,
                     ExecutableSpec
              From   IndexEntry
              Where  ExecutableSpec like {pattern};''
  ) >>= return . filterExec . allPackages
 where
  pattern = "%" ++ pat ++ "%"
  lpat    = map toLower pat

  filterExec = filter (\p -> lpat `isInfixOf` (map toLower $ execOfPackage p))

  toPackage (nm,vs,syn,cmp,exec) =
    emptyPackage { name = nm
                 , version = pkgRead vs
                 , synopsis = syn
                 , compilerCompatibility = pkgRead cmp
                 , executableSpec        = maybeToList (pkgRead exec)
                 }

--- Returns the complete repository where in each package
--- the name, version, synopsis, and compilerCompatibility is set.
getRepositoryWithNameVersionSynopsis :: Config -> ErrorLogger Repository
getRepositoryWithNameVersionSynopsis cfg = queryDBorCache cfg True $
  fmap (pkgsToRepository . map toPackage)
    ``sql* Select Name, Version, Synopsis, CompilerCompatibility
           From   IndexEntry;''
 where
  toPackage (nm,vs,syn,cmp) =
    emptyPackage { name = nm
                 , version = pkgRead vs
                 , synopsis = syn
                 , compilerCompatibility = pkgRead cmp
                 }

--- Returns the complete repository where in each package
--- the name, version, synopsis, dependencies and compilerCompatibility is set.
getRepositoryWithNameVersionSynopsisDeps :: Config -> ErrorLogger Repository
getRepositoryWithNameVersionSynopsisDeps cfg = queryDBorCache cfg True $
  fmap (pkgsToRepository . map toPackage)
    ``sql* Select Name, Version, Synopsis, Dependencies, CompilerCompatibility
           From   IndexEntry;''
 where
  toPackage (nm,vs,syn,deps,cmp) =
    emptyPackage { name = nm
                 , version = pkgRead vs
                 , synopsis = syn
                 , dependencies = pkgRead deps
                 , compilerCompatibility = pkgRead cmp
                 }

--- Returns the complete repository where in each package
--- the name, version, category, and compilerCompatibility is set.
getRepositoryWithNameVersionCategory :: Config -> ErrorLogger Repository
getRepositoryWithNameVersionCategory cfg = queryDBorCache cfg True $
  fmap (pkgsToRepository . map toPackage)
    ``sql* Select Name, Version, Category, CompilerCompatibility
           From   IndexEntry;''
 where
  toPackage (nm,vs,cats,cmp) =
    emptyPackage { name = nm
                 , version = pkgRead vs
                 , category = pkgRead cats
                 , compilerCompatibility = pkgRead cmp
                 }

--- Returns the complete repository where in each package
--- the name, version, dependencies, and compilerCompatibility is set.
--- The information is read either from the cache DB or from the cache file.
getBaseRepository :: Config -> ErrorLogger Repository
getBaseRepository cfg = queryDBorCache cfg False $
  fmap (pkgsToRepository . map toBasePackage)
    ``sql* Select Name, Version, Dependencies, CompilerCompatibility
           From   IndexEntry;''

--- Translate the (Name|Version|Dependencies|CompilerCompatibility) columns
--- of the cache DB into a package where the name, version, dependencies,
--- and compilerCompatibility is set.
toBasePackage :: (String,String,String,String) -> Package
toBasePackage (nm,vs,deps,cmp) =
  emptyPackage { name = nm
               , version = pkgRead vs
               , dependencies = pkgRead deps
               , compilerCompatibility = pkgRead cmp
               }

--- Returns the repository containing only packages with a given name where
--- in each package the name, version, dependencies, and compilerCompatibility
--- is set.
--- The information is read either from the cache DB or from the cache file.
getRepoPackagesWithName :: Config -> String -> ErrorLogger Repository
getRepoPackagesWithName cfg pn = queryDBorCache cfg False $
  fmap (pkgsToRepository . map toBasePackage)
    ``sql* Select Name, Version, Dependencies, CompilerCompatibility
           From   IndexEntry
           Where  Name = {pn} ;''

--- Returns the repository containing all packages and dependencies
--- (in all versions) mentioned in the given package specification.
--- In each package the name, version, dependencies, and compilerCompatibility
--- is set.
--- The information is read either from the cache DB or from the cache file.
getRepoForPackageSpec :: Config -> Package -> ErrorLogger Repository
getRepoForPackageSpec cfg pkgspec =
  getRepoForPackages cfg (name pkgspec : dependencyNames pkgspec)

--- Returns the repository containing only packages of the second argument
--- and all the packages on which they depend (including all versions).
--- In each package the name, version, dependencies, and compilerCompatibility
--- is set.
--- The information is read either from the cache DB or from the cache file.
getRepoForPackages :: Config -> [String] -> ErrorLogger Repository
getRepoForPackages cfg pkgnames = do
  dbexists <- liftIOEL $ doesFileExist (repositoryCacheDB cfg)
  if dbexists
    then do warnIfRepositoryOld cfg
            let dbfile = repositoryCacheDB cfg
            logDebug $ "Reading repository database '" ++ dbfile ++ "'..."
            repo <- queryPackagesFromDB pkgnames [] []
            logDebug $ "Finished reading repository database"
            return repo
    else readRepository cfg False
 where
  queryPackagesFromDB [] _ pkgs = return $ pkgsToRepository pkgs
  queryPackagesFromDB (pn:pns) lpns pkgs
   | pn `elem` lpns = queryPackagesFromDB pns lpns pkgs
   | otherwise      = do
     logDebug $ "Reading package versions of " ++ pn
     pnpkgs <- liftIOEL $ queryPackage pn
     let newdeps = concatMap dependencyNames pnpkgs
     queryPackagesFromDB (newdeps++pns) (pn:lpns) (pnpkgs++pkgs)

  queryPackage pn = runQueryOnDB (repositoryCacheDB cfg) $
    fmap (map toBasePackage)
    ``sql* Select Name, Version, Dependencies, CompilerCompatibility
           From IndexEntry
           Where Name = {pn} ;''

--- Retrieves all versions of a package with a given name from the repository.
---
--- @param cfg     - the current CPM configuration
--- @param pkgname - the package name to be retrieved
--- @param pre     - should pre-release versions be included?
getAllPackageVersions :: Config -> String -> Bool -> ErrorLogger [Package]
getAllPackageVersions cfg pkgname pre = do
  repo <- getRepoPackagesWithName cfg pkgname
  return (findAllVersions repo pkgname pre)

--- Retrieves a package with a given name and version from the repository.
---
--- @param cfg     - the current CPM configuration
--- @param pkgname - the package name to be retrieved
--- @param ver     - the requested version of the package
getPackageVersion :: Config -> String -> Version -> ErrorLogger (Maybe Package)
getPackageVersion cfg pkgname ver = do
  repo <- getRepoPackagesWithName cfg pkgname
  return (findVersion repo pkgname ver)

--- If the cache DB exists, run the DB query to get the repository,
--- otherwise read the (small or large) repository cache file.
queryDBorCache :: Config -> Bool -> DBAction Repository -> ErrorLogger Repository
queryDBorCache cfg large dbaction = do
  dbexists <- liftIOEL $ doesFileExist (repositoryCacheDB cfg)
  if dbexists then runQuery cfg dbaction
              else readRepository cfg large

--- Reads an unqualified Curry term w.r.t. the module `CPM.Package`.
pkgRead :: Data a => String -> a
pkgRead = readUnqualifiedTerm ["CPM.Package","Prelude"]

------------------------------------------------------------------------------
--- Adds a new package to the repository cache.
--- In the file-based implementation, we simply clean the cache files.
addPackageToRepositoryCache :: Config -> Package -> ErrorLogger ()
addPackageToRepositoryCache cfg pkg = do
  dbexists <- liftIOEL $ doesFileExist (repositoryCacheDB cfg)
  if dbexists then addPackagesToRepositoryDB cfg True [Left pkg]
              else cleanRepositoryCache cfg >> return ()

--- Updates an existing package in the repository cache.
--- In the file-based implementation, we simply clean the cache files.
updatePackageInRepositoryCache :: Config -> Package -> ErrorLogger ()
updatePackageInRepositoryCache cfg pkg = do
  dbexists <- liftIOEL $ doesFileExist (repositoryCacheDB cfg)
  if dbexists then removePackageFromRepositoryDB cfg pkg >>
                   addPackagesToRepositoryDB cfg True [Left pkg]
              else cleanRepositoryCache cfg >> return ()

--- Removes a package from the repository cache DB.
removePackageFromRepositoryDB :: Config -> Package -> ErrorLogger ()
removePackageFromRepositoryDB cfg pkg = runQuery cfg
  ``sql* Delete
         From   IndexEntry
         Where  Name = {name pkg} And Version = {showTerm (version pkg)};''

------------------------------------------------------------------------------
