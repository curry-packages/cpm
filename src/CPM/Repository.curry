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
  , emptyRepository, allPackages, pkgsToRepository
  , warnIfRepositoryOld, readRepositoryFrom
  , findAllVersions, findVersion, findLatestVersion
  , searchPackages, listPackages
  , useUpdateHelp, cleanRepositoryCache
  , readPackageFromRepository
  , repositoryCacheFilePrefix
  ) where

import Data.Char        ( toLower )
import Data.Either
import Data.List
import Data.Time
import Control.Monad
import System.Directory
import System.FilePath
import System.IO
import System.IOExts    ( readCompleteFile )
import System.Process   ( exitWith )

import CPM.Config        ( Config, repositoryDir )
import CPM.ConfigPackage ( packageVersion )
import CPM.ErrorLogger
import CPM.Package
import CPM.FileUtil      ( checkAndGetVisibleDirectoryContents
                         , copyDirectory, inDirectory
                         , quote, whenFileExists, removeDirectoryComplete )
import CPM.Resolution    ( isCompatibleToCompiler )

------------------------------------------------------------------------------
--- Abstract data type of a repository.
data Repository = Repository [Package]

--- Creates an empty repository.
emptyRepository :: Repository
emptyRepository = Repository []

--- Get all packages in the central package index.
allPackages :: Repository -> [Package]
allPackages (Repository ps) = ps

--- Construct a repository from a list of packages.
pkgsToRepository :: [Package] -> Repository
pkgsToRepository ps = Repository ps

------------------------------------------------------------------------------
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
findVersion repo pn v =
  maybeHead $ filter ((== v) . version) $ findAllVersions repo pn True
 where maybeHead []    = Nothing
       maybeHead (x:_) = Just x

------------------------------------------------------------------------------
-- Check status of repository index.

--- Prints a warning if the repository index is older than 10 days.
--- Abort with an error message if the repository index is not initialized
--- (e.g., by the command `cypm update`).
warnIfRepositoryOld :: Config -> ErrorLogger ()
warnIfRepositoryOld cfg = do
  logDebug "Check last update time of repository index..."
  let updatefile = repositoryDir cfg </> "README.md"
  updexists <- liftIOEL $ doesFileExist updatefile
  if updexists
    then do
      utime <- liftIOEL $ getModificationTime updatefile
      ctime <- liftIOEL $ getClockTime
      let warntime = addDays 10 utime
      when (compareClockTime ctime warntime == GT) $ do
        -- we assume that clock time is measured in seconds
        let timediff = clockTimeToInt ctime - clockTimeToInt utime
            days = timediff `div` (60*60*24)
        logInfo $ "Warning: your repository index is older than " ++
                      show days ++ " days.\n" ++ useUpdateHelp
    else do
      logError $ "It seems that the repository of CPM is not initialized.\n" ++
                 useUpdateHelp
      liftIOEL $ exitWith 1

useUpdateHelp :: String
useUpdateHelp = "Use 'cypm update' to download the newest package index."

--- Reads all package specifications from a repository.
--- If some errors occur, show them and terminate with error exit status.
---
--- @param path the location of the repository
--- @return repository
readRepositoryFrom :: String -> ErrorLogger Repository
readRepositoryFrom path = do
  (repo, repoErrors) <- tryReadRepositoryFrom path
  if null repoErrors
    then return repo
    else do logError "Problems while reading the package index:"
            mapM_ logError repoErrors
            liftIOEL $ exitWith 1

--- Reads all package specifications from a repository.
---
--- @param path the location of the repository
--- @return repository and possible repository reading errors
tryReadRepositoryFrom :: String -> ErrorLogger (Repository, [String])
tryReadRepositoryFrom path = do
  logDebug $ "Reading repository index from '" ++ path ++ "'..."
  repos     <- liftIOEL $ checkAndGetVisibleDirectoryContents path
  pkgPaths  <- liftIOEL $ mapM getDir (map (path </>) repos) >>= return . concat
  verDirs   <- liftIOEL $ mapM checkAndGetVisibleDirectoryContents pkgPaths
  verPaths  <- return $ concatMap (\ (d, p) -> map (d </>) p)
                     $ zip pkgPaths verDirs
  specPaths <- return $ map (</> packageSpecFile) verPaths
  logInfo "Reading repository index..."
  specs     <- liftIOEL $ mapM readPackageFile specPaths
  when (null (lefts specs)) $ logDebug "Finished reading repository"
  return $ (Repository $ rights specs, lefts specs)
 where
  readPackageFile f = do
    spec <- readPackageSpec <$> readCompleteFile f
    seq (id $!! spec) (putChar '.' >> hFlush stdout)
    return $ case spec of
      Left err -> Left $ "Problem reading '" ++ f ++ "': " ++ err
      Right  s -> Right s

  getDir d = doesDirectoryExist d >>= \b -> return $ if b then [d] else []


------------------------------------------------------------------------------
--- The prefix of all file names implementing the repository cache.
repositoryCacheFilePrefix :: Config -> String
repositoryCacheFilePrefix cfg = repositoryDir cfg </> "REPOSITORY_CACHE"

--- Cleans the repository cache.
cleanRepositoryCache :: Config -> ErrorLogger ()
cleanRepositoryCache cfg = do
  logDebug $ "Cleaning repository cache '" ++
                 repositoryCacheFilePrefix cfg ++ "*'"
  showExecCmd $ "/bin/rm -f " ++ quote (repositoryCacheFilePrefix cfg) ++ "*"
  return ()

------------------------------------------------------------------------------
--- Reads a given package from the default repository directory.
--- This is useful to obtain the complete package specification
--- from a possibly incomplete package specification.
readPackageFromRepository :: Config -> Package -> ErrorLogger Package
readPackageFromRepository cfg pkg =
  let pkgdir = repositoryDir cfg </> name pkg </> showVersion (version pkg)
  in loadPackageSpec pkgdir

------------------------------------------------------------------------------
