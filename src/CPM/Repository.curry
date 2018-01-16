------------------------------------------------------------------------------
--- This module implements functionality surrounding the package *repository*.
--- The repository is the index of all packages known to the package manager.
--- It contains metadata about the packages, such as their names, versions 
--- dependencies and where they can be acquired. The repository does not contain
--- the actual packages. For a list of packages that are currently installed 
--- locally, you can consult the *database*.
------------------------------------------------------------------------------

module CPM.Repository 
  ( Repository
  , emptyRepository
  , allPackages
  , readRepository
  , findAllVersions, findVersion, findLatestVersion
  , searchPackages
  , listPackages
  , useUpdateHelp, updateRepository, cleanRepositoryCache
  , readPackageFromRepository, getAllPackageVersions, getPackageVersion
  ) where

import Char         ( toLower )
import Directory
import Either
import FilePath
import IO
import IOExts       ( readCompleteFile )
import List
import ReadShowTerm ( showQTerm, readQTerm, showTerm, readUnqualifiedTerm )
import System       ( exitWith, system )
import Time

import CPM.Config        ( Config, repositoryDir, packageIndexRepository
                         , packageInstallDir )
import CPM.ConfigPackage ( packageVersion )
import CPM.ErrorLogger
import CPM.Package
import CPM.FileUtil      ( checkAndGetVisibleDirectoryContents, inDirectory
                         , whenFileExists, removeDirectoryComplete )
import CPM.Resolution    ( isCompatibleToCompiler )

data Repository = Repository [Package]

--- Creates an empty repository.
emptyRepository :: Repository
emptyRepository = Repository []

--- Finds all versions of a package known to the repository. Returns the 
--- packages sorted from newest to oldest.
---
--- @param r the repository
--- @param p the name of the package to search for
--- @param pre should pre-release versions be included?
findAllVersions :: Repository -> String -> Bool -> [Package]
findAllVersions (Repository ps) p pre =
  sortedByVersion $ preFiltered $ sameName ps
 where 
  sortedByVersion = sortBy (\a b -> (version a) `vgt` (version b))
  preFiltered = filter filterPre
  sameName = filter ((== p) . name) 
  filterPre p' = pre || (not . isPreRelease . version) p'

--- Search the names and synopses of all compiler-compatbile packages
--- in the repository for a particular term.
--- Lower/upercase is ignored for the search.
--- Returns all matching versions (newest first) of each package.
---
--- @param repo       - the repository
--- @param searchmod  - search for some module?
--- @param searchexec - search for some executable?
--- @param searchterm - the term to search for
searchPackages :: Repository -> Bool -> Bool -> String -> [[Package]]
searchPackages (Repository ps) searchmod searchexec searchterm =
  map sortedByVersion (groupBy (\a b -> name a == name b) allResults)
 where
  allResults = let s = lowerS searchterm
               in if searchmod
                    then filter (\p -> searchterm `elem` exportedModules p) ps
                    else if searchexec
                           then filter (\p -> s `isInfixOf`
                                                (lowerS $ execOfPackage p)) ps
                           else filter (matches s) ps

  matches q p = q `isInfixOf` (lowerS $ name p) ||
                q `isInfixOf` (lowerS $ synopsis p) ||
                q `isInfixOf` (lowerS $ unwords (exportedModules p))

  sortedByVersion = sortBy (\a b -> version a `vgt` version b)

  lowerS = map toLower


--- Get all packages in the repository and group them by versions
--- (newest first).
---
--- @param cfg  - the current CPM configuration
--- @param repo - the repository
listPackages :: Repository -> [[Package]]
listPackages (Repository ps) =
  map sortedByVersion (groupBy (\a b -> name a == name b) ps)
 where
  sortedByVersion = sortBy (\a b -> (version a) `vgt` (version b))

--- Finds the latest compiler-compatbile version of a package.
---
--- @param cfg  - the current CPM configuration
--- @param repo - the central package index
--- @param p - the package to search for
--- @param pre - include pre-release versions
findLatestVersion :: Config -> Repository -> String -> Bool -> Maybe Package
findLatestVersion cfg repo pn pre =
 case filter (isCompatibleToCompiler cfg) (findAllVersions repo pn pre) of
  []    -> Nothing
  (p:_) -> Just p

--- Finds a specific version of a package.
findVersion :: Repository -> String -> Version -> Maybe Package
findVersion repo p v =
  maybeHead $ filter ((== v) . version) $ findAllVersions repo p True
 where maybeHead []    = Nothing
       maybeHead (x:_) = Just x

--- Get all packages in the central package index.
allPackages :: Repository -> [Package]
allPackages (Repository ps) = ps

--- Reads all package specifications from the default repository.
--- Uses the cache if it is present or update the cache after reading.
--- If some errors occur, show them and terminate with error exit status.
---
--- @param cfg   - the configuration to use
--- @param large - if true reads the larger cache with more package information
---                (e.g., for searching all packages)
readRepository :: Config -> Bool -> IO Repository
readRepository cfg large = do
  warnOldRepo cfg
  mbrepo <- readRepositoryCache cfg large
  case mbrepo of
    Nothing -> do
      infoMessage $ "Writing " ++ (if large then "large " else "") ++
                    "repository cache..."
      (repo, repoErrors) <- readRepositoryFrom (repositoryDir cfg)
      if null repoErrors
        then writeRepositoryCache cfg large repo >> return repo
        else do errorMessage "Problems while reading the package index:"
                mapM_ errorMessage repoErrors
                exitWith 1
    Just repo -> return repo

--- Sets the date of the last update by touching README.md.
setLastUpdate :: Config -> IO ()
setLastUpdate cfg =
  system (unwords ["touch", repositoryDir cfg </> "README.md"]) >> done

--- Prints a warning if the repository index is older than 10 days.
warnOldRepo :: Config -> IO ()
warnOldRepo cfg = do
  let updatefile = repositoryDir cfg </> "README.md"
  updexists <- doesFileExist updatefile
  if updexists
    then do
      utime <- getModificationTime updatefile
      ctime <- getClockTime
      let warntime = addDays 10 utime
      when (compareClockTime ctime warntime == GT) $ do
        -- we assume that clock time is measured in seconds
        let timediff = clockTimeToInt ctime - clockTimeToInt utime
            days = timediff `div` (60*60*24)
        infoMessage $ "Warning: your repository index is older than " ++
                      show days ++ " days.\n" ++ useUpdateHelp
    else infoMessage $ "Warning: your repository index is not up-to-date.\n" ++
                       useUpdateHelp

useUpdateHelp :: String
useUpdateHelp = "Use 'cypm update' to download the newest package index."

--- Reads all package specifications from a repository.
---
--- @param path the location of the repository
readRepositoryFrom :: String -> IO (Repository, [String])
readRepositoryFrom path = do
  debugMessage $ "Reading repository index from '" ++ path ++ "'..."
  repos     <- checkAndGetVisibleDirectoryContents path
  pkgPaths  <- mapIO getDir (map (path </>) repos) >>= return . concat
  verDirs   <- mapIO checkAndGetVisibleDirectoryContents pkgPaths
  verPaths  <- return $ concatMap (\ (d, p) -> map (d </>) p)
                     $ zip pkgPaths verDirs
  specPaths <- return $ map (</> "package.json") verPaths
  specs     <- mapIO readPackageFile specPaths
  when (null (lefts specs)) $ debugMessage "Finished reading repository"
  return $ (Repository $ rights specs, lefts specs)
 where
  readPackageSpecIO = liftIO readPackageSpec

  readPackageFile f = do
    spec <- readPackageSpecIO $ readCompleteFile f
    return $ case spec of
      Left err -> Left $ "Problem reading '" ++ f ++ "': " ++ err
      Right  s -> Right s

  getDir d = doesDirectoryExist d >>= \b -> return $ if b then [d] else []


--- Updates the package index from the central Git repository.
--- Cleans also the global package cache in order to support
--- downloading the newest versions.
updateRepository :: Config -> IO (ErrorLogger ())
updateRepository cfg = do
  cleanRepositoryCache cfg
  debugMessage $ "Deleting global package cache: '" ++
                 packageInstallDir cfg ++ "'"
  removeDirectoryComplete (packageInstallDir cfg)
  gitExists <- doesDirectoryExist $ (repositoryDir cfg) </> ".git"
  if gitExists 
    then do
      c <- inDirectory (repositoryDir cfg) $ execQuietCmd $ cleanPullCmd
      if c == 0
        then do setLastUpdate cfg
                cleanRepositoryCache cfg
                log Info "Successfully updated repository"
        else failIO $ "Failed to update git repository, return code " ++ show c
    else do
      c <- inDirectory (repositoryDir cfg) $ execQuietCmd cloneCommand
      if c == 0
        then do setLastUpdate cfg
                cleanRepositoryCache cfg
                log Info "Successfully updated repository"
        else failIO $ "Failed to update git repository, return code " ++ show c
 where
  cleanPullCmd q = "git clean -d -f && git pull " ++ q ++ " origin master"
  cloneCommand q = unwords ["git clone", q, packageIndexRepository cfg, "."]


------------------------------------------------------------------------------
-- Operations implementing the repository cache for faster reading.
-- The repository cache contains reduced package specifications
-- for faster reading/writing by removing some information
-- which is not relevant for the repository data structure.
--
-- The relevant package fields are:
-- * small cache: name version dependencies compilerCompatibility
-- * large cache: synopsis category sourceDirs exportedModules executableSpec

--- The local file name containing the repository cache as a Curry term.
repositoryCacheFileName :: String
repositoryCacheFileName = "REPOSITORY_CACHE"

--- The file containing the repository cache as a Curry term.
repositoryCache :: Config -> Bool -> String
repositoryCache cfg large =
  repositoryDir cfg </> repositoryCacheFileName ++
  (if large then "_LARGE" else "")

--- The first line of the repository cache (to check version compatibility):
repoCacheVersion :: String
repoCacheVersion = packageVersion ++ "-1"

--- Stores the given repository in the cache.
---
--- @param cfg   - the configuration to use
--- @param large - if true writes the larger cache with more package information
---                (e.g., for searching all packages)
--- @param repo  - the repository to write
writeRepositoryCache :: Config -> Bool -> Repository -> IO ()
writeRepositoryCache cfg large repo =
  writeFile (repositoryCache cfg large) $ unlines $
    repoCacheVersion :
    map (if large then showTerm . package2largetuple
                  else showTerm . package2smalltuple)
        (allPackages repo)
 where
  package2smalltuple p =
    ( name p, version p, dependencies p, compilerCompatibility p )

  package2largetuple p =
    (package2smalltuple p,
    (synopsis p, category p, sourceDirs p, exportedModules p,
     executableSpec  p))

--- Reads the given repository from the cache.
---
--- @param cfg   - the configuration to use
--- @param large - if true reads the larger cache with more package information
---                (e.g., for searching all packages)
readRepositoryCache :: Config -> Bool -> IO (Maybe Repository)
readRepositoryCache cfg large = do
  let cf = repositoryCache cfg large
  excache <- doesFileExist cf
  if excache
    then debugMessage ("Reading repository cache from '" ++ cf ++ "'...") >>
         catch ((if large
                  then readTermInCacheFile cfg (largetuple2package . uread) cf
                  else readTermInCacheFile cfg (smalltuple2package . uread) cf)
                  >>= \repo ->
                debugMessage "Finished reading repository cache" >> return repo)
               (\_ -> do infoMessage "Cleaning broken repository cache..."
                         cleanRepositoryCache cfg
                         return Nothing )
    else return Nothing
 where
  uread s = readUnqualifiedTerm ["CPM.Package","Prelude"] s

  smalltuple2package (nm,vs,dep,cmp) =
    emptyPackage { name = nm
                 , version = vs
                 , dependencies = dep
                 , compilerCompatibility = cmp
                 }

  largetuple2package (basics,(sy,cat,srcs,exps,exec)) =
    (smalltuple2package basics)
      { synopsis = sy
      , category = cat
      , sourceDirs = srcs
      , exportedModules = exps
      , executableSpec  = exec
      }

readTermInCacheFile :: Config -> (String -> Package) -> String
                    -> IO (Maybe Repository)
readTermInCacheFile cfg trans cf = do
  h <- openFile cf ReadMode
  pv <- hGetLine h
  if pv == repoCacheVersion
    then hGetContents h >>= \t ->
         return $!! Just (Repository (map trans (lines  t)))
    else do infoMessage "Cleaning repository cache (wrong version)..."
            cleanRepositoryCache cfg
            return Nothing

--- Cleans the repository cache.
cleanRepositoryCache :: Config -> IO ()
cleanRepositoryCache cfg = do
  let smallcachefile = repositoryCache cfg False
      largecachefile = repositoryCache cfg True
  whenFileExists smallcachefile $ removeFile smallcachefile
  whenFileExists largecachefile $ removeFile largecachefile

------------------------------------------------------------------------------
--- Reads a given package from the default repository directory.
--- This is useful to obtain the complete package specification
--- from a possibly incomplete package specification.
readPackageFromRepository :: Config -> Package -> IO (ErrorLogger Package)
readPackageFromRepository cfg pkg =
  let pkgdir = repositoryDir cfg </> name pkg </> showVersion (version pkg)
  in loadPackageSpec pkgdir

------------------------------------------------------------------------------
-- Some operations to access the repository. In the future, these
-- operations might be implemented by a better database structure
-- of the repository to speed-up the repository access.

--- Retrieves all versions of a package with a given name from the repository.
---
--- @param cfg     - the current CPM configuration
--- @param pkgname - the package name to be retrieved
--- @param pre     - should pre-release versions be included?
getAllPackageVersions :: Config -> String -> Bool -> IO [Package]
getAllPackageVersions cfg pkgname pre = do
  repo <- readRepository cfg False
  return (findAllVersions repo pkgname pre)

--- Retrieves a package with a given name and version from the repository.
---
--- @param cfg     - the current CPM configuration
--- @param pkgname - the package name to be retrieved
--- @param ver     - the requested version of the package
getPackageVersion :: Config -> String -> Version -> IO (Maybe Package)
getPackageVersion cfg pkgname ver = do
  repo <- readRepository cfg False
  return (findVersion repo pkgname ver)

------------------------------------------------------------------------------
