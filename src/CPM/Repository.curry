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
  , useUpdateHelp, updateRepository, updateRepositoryCache
  , readPackageFromRepository, getAllPackageVersions, getPackageVersion
  ) where

import Char         ( toLower )
import Directory
import Either
import FilePath
import IO
import IOExts       ( readCompleteFile )
import List
import ReadShowTerm ( showQTerm, readQTerm )
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
--- @param cfg the configuration to use
readRepository :: Config -> IO Repository
readRepository cfg = do
  warnOldRepo cfg
  mbrepo <- readRepositoryCache cfg
  case mbrepo of
    Nothing -> do
      infoMessage "Writing repository cache..."
      (repo, repoErrors) <- readRepositoryFrom (repositoryDir cfg)
      if null repoErrors
        then writeRepositoryCache cfg repo >> return repo
        else do putStrLn "Problems while reading the package index:"
                mapIO putStrLn repoErrors
                exitWith 1
    Just repo -> return repo

--- Sets the date of the last update by touching README.md.
setLastUpdate :: Config -> IO ()
setLastUpdate cfg =
  system (unwords ["touch", repositoryDir cfg </> "README.md"]) >> done

--- Prints a warning if the repository index is older than 10 days.
warnOldRepo :: Config -> IO ()
warnOldRepo cfg = do
  utime <- getModificationTime (repositoryDir cfg </> "README.md")
  ctime <- getClockTime
  let warntime = addDays 10 utime
  when (compareClockTime ctime warntime == GT) $ do
    -- We assume that clock time is measured in seconds (as in PAKCS or KiCS2)
    let timediff = clockTimeToInt ctime - clockTimeToInt utime
        days = timediff `div` (60*60*24)
    putStrLn $  "Warning: your repository index is older than " ++
                show days ++ " days."
    putStrLn useUpdateHelp

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
                updateRepositoryCache cfg
                log Info "Successfully updated repository"
        else failIO $ "Failed to update git repository, return code " ++ show c
    else do
      c <- inDirectory (repositoryDir cfg) $ execQuietCmd cloneCommand
      if c == 0
        then do setLastUpdate cfg
                updateRepositoryCache cfg
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
-- name version synopsis category dependencies
-- compilerCompatibility sourceDirs exportedModules

--- The local file name containing the repository cache as a Curry term.
repositoryCacheFileName :: String
repositoryCacheFileName = "REPOSITORY_CACHE"

--- The file containing the repository cache as a Curry term.
repositoryCache :: Config -> String
repositoryCache cfg = repositoryDir cfg </> repositoryCacheFileName

--- Updates the repository cache with the current repository index.
updateRepositoryCache :: Config -> IO ()
updateRepositoryCache cfg = do
  cleanRepositoryCache cfg
  repo <- readRepository cfg
  writeRepositoryCache cfg repo

--- Stores the given repository in the cache.
writeRepositoryCache :: Config -> Repository -> IO ()
writeRepositoryCache cfg repo =
  writeFile (repositoryCache cfg)
            (packageVersion ++ "\n" ++
             showQTerm (map package2tuple (allPackages repo)))
 where
  package2tuple p =
    ( name p
    , version p
    , synopsis p
    , category p
    , dependencies p
    , compilerCompatibility p
    , sourceDirs p
    , exportedModules p
    )

--- Reads the given repository from the cache.
readRepositoryCache :: Config -> IO (Maybe Repository)
readRepositoryCache cfg = do
  let cf = repositoryCache cfg
  excache <- doesFileExist cf
  if excache
    then debugMessage ("Reading repository cache from '" ++ cf ++ "'...") >>
         catch (readTermInCacheFile cf >>= \repo ->
                debugMessage "Finished reading repository cache" >> return repo)
               (\_ -> do infoMessage "Cleaning broken repository cache..."
                         cleanRepositoryCache cfg
                         return Nothing )
    else return Nothing
 where
  readTermInCacheFile cf = do
    h <- openFile cf ReadMode
    pv <- hGetLine h
    if pv == packageVersion
      then hGetContents h >>= \t ->
           return $!! Just (Repository (map tuple2package (readQTerm t)))
      else do infoMessage "Cleaning repository cache (wrong version)..."
              cleanRepositoryCache cfg
              return Nothing

  tuple2package (nm,vs,sy,cat,dep,cmp,srcs,exps) =
    emptyPackage { name = nm
                 , version = vs
                 , synopsis = sy
                 , category = cat
                 , dependencies = dep
                 , compilerCompatibility = cmp
                 , sourceDirs = srcs
                 , exportedModules = exps
                 }


--- Cleans the repository cache.
cleanRepositoryCache :: Config -> IO ()
cleanRepositoryCache cfg = do
  let cachefile = repositoryCache cfg
  whenFileExists cachefile $ removeFile cachefile

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
getAllPackageVersions cfg pkgname pre =
  readRepository cfg >>= \repo -> return (findAllVersions repo pkgname pre)

--- Retrieves a package with a given name and version from the repository.
---
--- @param cfg     - the current CPM configuration
--- @param pkgname - the package name to be retrieved
--- @param ver     - the requested version of the package
getPackageVersion :: Config -> String -> Version -> IO (Maybe Package)
getPackageVersion cfg pkgname ver =
  readRepository cfg >>= \repo -> return (findVersion repo pkgname ver)

------------------------------------------------------------------------------
