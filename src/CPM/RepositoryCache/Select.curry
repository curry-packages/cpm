------------------------------------------------------------------------------
--- Some queries on the repository cache database.
---
--- @author Michael Hanus
--- @version March 2018
------------------------------------------------------------------------------
 

module CPM.RepositoryCache.Select
  ( searchNameSynopsisModules
  , searchExportedModules, searchExecutable
  , getRepositoryWithNameVersionSynopsis
  , getRepositoryWithNameVersionCategory
  , getBaseRepository
  , getRepoForPackageSpec
  , getRepoForPackages
  , getAllPackageVersions, getPackageVersion
  )
 where

import Char         ( toLower )
import Directory    ( doesFileExist )
import List         ( isInfixOf )
import ReadShowTerm

import Database.CDBI.ER 
import Database.CDBI.Connection

import CPM.Config      ( Config )
import CPM.ErrorLogger
import CPM.FileUtil    ( ifFileExists )
import CPM.RepositoryCache.RepositoryDB
import CPM.RepositoryCache.File ( readRepository )
import CPM.RepositoryCache.Init ( repositoryCacheDB )
import CPM.Package
import CPM.Repository

--- Runs a query on the repository cache DB and show debug infos.
runQuery :: Config -> DBAction a -> IO a
runQuery cfg dbact = do
  warnIfRepositoryOld cfg
  let dbfile = repositoryCacheDB cfg
  debugMessage $ "Reading repository database '" ++ dbfile ++ "'..."
  result <- runQueryOnDB dbfile dbact
  debugMessage $ "Finished reading repository database"
  return result

--- Returns the packages of the repository containing a given string
--- in the name, synopsis, or exported modules.
--- In each package, the name, version, synopsis, and compilerCompatibility
--- is set.
searchNameSynopsisModules :: Config -> String -> IO [Package]
searchNameSynopsisModules cfg pat =
  runQuery cfg $ liftM (map toPackage)
    (Database.CDBI.ER.getColumnFourTuple [] [Database.CDBI.ER.FourCS Database.CDBI.ER.All (Database.CDBI.ER.fourCol (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryNameColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryVersionColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntrySynopsisColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryCompilerCompatibilityColDesc 0 Database.CDBI.ER.none)) (Database.CDBI.ER.TC CPM.RepositoryCache.RepositoryDB.indexEntryTable 0 Nothing) (Database.CDBI.ER.Criteria (Database.CDBI.ER.Or [Database.CDBI.ER.like (Database.CDBI.ER.colNum CPM.RepositoryCache.RepositoryDB.indexEntryColumnName 0) (Database.CDBI.ER.string (pattern)) ,Database.CDBI.ER.Or [Database.CDBI.ER.like (Database.CDBI.ER.colNum CPM.RepositoryCache.RepositoryDB.indexEntryColumnSynopsis 0) (Database.CDBI.ER.string (pattern)) ,Database.CDBI.ER.like (Database.CDBI.ER.colNum CPM.RepositoryCache.RepositoryDB.indexEntryColumnExportedModules 0) (Database.CDBI.ER.string (pattern))]]) Nothing)] [] Nothing)




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
searchExportedModules :: Config -> String -> IO [Package]
searchExportedModules cfg pat =
  (queryDBorCache cfg True $
     liftM (pkgsToRepository . map toPackage)
       (Database.CDBI.ER.getColumnFiveTuple [] [Database.CDBI.ER.FiveCS Database.CDBI.ER.All (Database.CDBI.ER.fiveCol (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryNameColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryVersionColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntrySynopsisColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryCompilerCompatibilityColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryExportedModulesColDesc 0 Database.CDBI.ER.none)) (Database.CDBI.ER.TC CPM.RepositoryCache.RepositoryDB.indexEntryTable 0 Nothing) (Database.CDBI.ER.Criteria (Database.CDBI.ER.like (Database.CDBI.ER.colNum CPM.RepositoryCache.RepositoryDB.indexEntryColumnExportedModules 0) (Database.CDBI.ER.string (pattern))) Nothing)] [] Nothing)



  ) >>= return . filterExpModules . allPackages
 where
  pattern = "%" ++ pat ++ "%"

  filterExpModules = filter (\p -> pat `elem` exportedModules p)
  
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
searchExecutable :: Config -> String -> IO [Package]
searchExecutable cfg pat =
  (queryDBorCache cfg True $
     liftM (pkgsToRepository . map toPackage)
       (Database.CDBI.ER.getColumnFiveTuple [] [Database.CDBI.ER.FiveCS Database.CDBI.ER.All (Database.CDBI.ER.fiveCol (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryNameColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryVersionColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntrySynopsisColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryCompilerCompatibilityColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryExecutableSpecColDesc 0 Database.CDBI.ER.none)) (Database.CDBI.ER.TC CPM.RepositoryCache.RepositoryDB.indexEntryTable 0 Nothing) (Database.CDBI.ER.Criteria (Database.CDBI.ER.like (Database.CDBI.ER.colNum CPM.RepositoryCache.RepositoryDB.indexEntryColumnExecutableSpec 0) (Database.CDBI.ER.string (pattern))) Nothing)] [] Nothing)



  ) >>= return . filterExec . allPackages
 where
  pattern = "%" ++ pat ++ "%"
  s = map toLower pat

  filterExec = filter (\p -> s `isInfixOf` (map toLower $ execOfPackage p))
  
  toPackage (nm,vs,syn,cmp,exec) =
    emptyPackage { name = nm
                 , version = pkgRead vs
                 , synopsis = syn
                 , compilerCompatibility = pkgRead cmp
                 , executableSpec        = pkgRead exec
                 }

--- Returns the complete repository where in each package
--- the name, version, synopsis, and compilerCompatibility is set.
getRepositoryWithNameVersionSynopsis :: Config -> IO Repository
getRepositoryWithNameVersionSynopsis cfg = queryDBorCache cfg True $
  liftM (pkgsToRepository . map toPackage)
    (Database.CDBI.ER.getColumnFourTuple [] [Database.CDBI.ER.FourCS Database.CDBI.ER.All (Database.CDBI.ER.fourCol (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryNameColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryVersionColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntrySynopsisColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryCompilerCompatibilityColDesc 0 Database.CDBI.ER.none)) (Database.CDBI.ER.TC CPM.RepositoryCache.RepositoryDB.indexEntryTable 0 Nothing) (Database.CDBI.ER.Criteria Database.CDBI.ER.None Nothing)] [] Nothing)

 where
  toPackage (nm,vs,syn,cmp) =
    emptyPackage { name = nm
                 , version = pkgRead vs
                 , synopsis = syn
                 , compilerCompatibility = pkgRead cmp
                 }

--- Returns the complete repository where in each package
--- the name, version, category, and compilerCompatibility is set.
getRepositoryWithNameVersionCategory :: Config -> IO Repository
getRepositoryWithNameVersionCategory cfg = queryDBorCache cfg True $
  liftM (pkgsToRepository . map toPackage)
    (Database.CDBI.ER.getColumnFourTuple [] [Database.CDBI.ER.FourCS Database.CDBI.ER.All (Database.CDBI.ER.fourCol (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryNameColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryVersionColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryCategoryColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryCompilerCompatibilityColDesc 0 Database.CDBI.ER.none)) (Database.CDBI.ER.TC CPM.RepositoryCache.RepositoryDB.indexEntryTable 0 Nothing) (Database.CDBI.ER.Criteria Database.CDBI.ER.None Nothing)] [] Nothing)

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
getBaseRepository :: Config -> IO Repository
getBaseRepository cfg = queryDBorCache cfg False $
  liftM (pkgsToRepository . map toBasePackage)
    (Database.CDBI.ER.getColumnFourTuple [] [Database.CDBI.ER.FourCS Database.CDBI.ER.All (Database.CDBI.ER.fourCol (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryNameColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryVersionColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryDependenciesColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryCompilerCompatibilityColDesc 0 Database.CDBI.ER.none)) (Database.CDBI.ER.TC CPM.RepositoryCache.RepositoryDB.indexEntryTable 0 Nothing) (Database.CDBI.ER.Criteria Database.CDBI.ER.None Nothing)] [] Nothing)


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
getRepoPackagesWithName :: Config -> String -> IO Repository
getRepoPackagesWithName cfg pn = queryDBorCache cfg False $
  liftM (pkgsToRepository . map toBasePackage)
    (Database.CDBI.ER.getColumnFourTuple [] [Database.CDBI.ER.FourCS Database.CDBI.ER.All (Database.CDBI.ER.fourCol (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryNameColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryVersionColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryDependenciesColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryCompilerCompatibilityColDesc 0 Database.CDBI.ER.none)) (Database.CDBI.ER.TC CPM.RepositoryCache.RepositoryDB.indexEntryTable 0 Nothing) (Database.CDBI.ER.Criteria (Database.CDBI.ER.equal (Database.CDBI.ER.colNum CPM.RepositoryCache.RepositoryDB.indexEntryColumnName 0) (Database.CDBI.ER.string (pn))) Nothing)] [] Nothing)



--- Returns the repository containing all packages and dependencies
--- (in all versions) mentioned in the given package specification.
--- In each package the name, version, dependencies, and compilerCompatibility
--- is set.
--- The information is read either from the cache DB or from the cache file.
getRepoForPackageSpec :: Config -> Package -> IO Repository
getRepoForPackageSpec cfg pkgspec =
  getRepoForPackages cfg (name pkgspec : dependencyNames pkgspec)

--- Returns the repository containing only packages of the second argument
--- and all the packages on which they depend (including all versions).
--- In each package the name, version, dependencies, and compilerCompatibility
--- is set.
--- The information is read either from the cache DB or from the cache file.
getRepoForPackages :: Config -> [String] -> IO Repository
getRepoForPackages cfg pkgnames = do
  dbexists <- doesFileExist (repositoryCacheDB cfg)
  if dbexists
    then do warnIfRepositoryOld cfg
            let dbfile = repositoryCacheDB cfg
            debugMessage $ "Reading repository database '" ++ dbfile ++ "'..."
            repo <- queryPackagesFromDB pkgnames [] []
            debugMessage $ "Finished reading repository database"
            return repo
    else readRepository cfg False
 where
  queryPackagesFromDB [] _ pkgs = return $ pkgsToRepository pkgs
  queryPackagesFromDB (pn:pns) lpns pkgs
   | pn `elem` lpns = queryPackagesFromDB pns lpns pkgs
   | otherwise      = do
     debugMessage $ "Reading package versions of " ++ pn
     pnpkgs <- queryPackage pn
     let newdeps = concatMap dependencyNames pnpkgs
     queryPackagesFromDB (newdeps++pns) (pn:lpns) (pnpkgs++pkgs)

  queryPackage pn = runQueryOnDB (repositoryCacheDB cfg) $
    liftM (map toBasePackage)
    (Database.CDBI.ER.getColumnFourTuple [] [Database.CDBI.ER.FourCS Database.CDBI.ER.All (Database.CDBI.ER.fourCol (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryNameColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryVersionColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryDependenciesColDesc 0 Database.CDBI.ER.none) (Database.CDBI.ER.singleCol CPM.RepositoryCache.RepositoryDB.indexEntryCompilerCompatibilityColDesc 0 Database.CDBI.ER.none)) (Database.CDBI.ER.TC CPM.RepositoryCache.RepositoryDB.indexEntryTable 0 Nothing) (Database.CDBI.ER.Criteria (Database.CDBI.ER.equal (Database.CDBI.ER.colNum CPM.RepositoryCache.RepositoryDB.indexEntryColumnName 0) (Database.CDBI.ER.string (pn))) Nothing)] [] Nothing)



--- Retrieves all versions of a package with a given name from the repository.
---
--- @param cfg     - the current CPM configuration
--- @param pkgname - the package name to be retrieved
--- @param pre     - should pre-release versions be included?
getAllPackageVersions :: Config -> String -> Bool -> IO [Package]
getAllPackageVersions cfg pkgname pre = do
  repo <- getRepoPackagesWithName cfg pkgname
  return (findAllVersions repo pkgname pre)

--- Retrieves a package with a given name and version from the repository.
---
--- @param cfg     - the current CPM configuration
--- @param pkgname - the package name to be retrieved
--- @param ver     - the requested version of the package
getPackageVersion :: Config -> String -> Version -> IO (Maybe Package)
getPackageVersion cfg pkgname ver = do
  repo <- getRepoPackagesWithName cfg pkgname
  return (findVersion repo pkgname ver)


--- If the cache DB exists, run the DB query to get the repository,
--- otherwise read the (small or large) repository cache file.
queryDBorCache :: Config -> Bool -> DBAction Repository -> IO Repository
queryDBorCache cfg large dbaction = do
  dbexists <- doesFileExist (repositoryCacheDB cfg)
  if dbexists then runQuery cfg dbaction
              else readRepository cfg large

--- Reads an unqualified Curry term w.r.t. the module `CPM.Package`.
pkgRead :: String -> a
pkgRead = readUnqualifiedTerm ["CPM.Package","Prelude"]
