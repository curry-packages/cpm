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
  , findVersion
  , findLatestVersion
  , searchPackages
  , listPackages
  , updateRepository
  , updateRepositoryCache
  ) where

import Char         ( toLower, toUpper )
import Directory
import Either
import FilePath
import IO
import IOExts       ( readCompleteFile )
import List
import ReadShowTerm ( showQTerm, readQTerm )
import System       ( exitWith )

import CPM.Config        ( Config, repositoryDir, packageIndexRepository
                         , packageInstallDir )
import CPM.ConfigPackage ( packageVersion )
import CPM.ErrorLogger
import CPM.Package
import CPM.FileUtil      ( checkAndGetDirectoryContents, inDirectory
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
                    then filter (\p -> s `elem` exportedModules p) ps
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
--- Use the cache if is present or update the cache after reading.
--- If some errors occur, show them and terminate with exit status.
---
--- @param cfg the configuration to use
readRepository :: Config -> IO Repository
readRepository cfg = do
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

--- Reads all package specifications from a repository.
---
--- @param path the location of the repository
readRepositoryFrom :: String -> IO (Repository, [String])
readRepositoryFrom path = do
  debugMessage $ "Reading repository index from '" ++ path ++ "'..."
  pkgDirs <- checkAndGetDirectoryContents path
  pkgPaths <- return $ map (path </>) $ filter dirOrSpec pkgDirs
  verDirs <- mapIO checkAndGetDirectoryContents pkgPaths
  verPaths <- return $ concatMap (\ (d, p) -> map (d </>) (filter dirOrSpec p))
                     $ zip pkgPaths verDirs
  specPaths <- return $ map (</> "package.json") verPaths
  specs <- mapIO readPackageFile specPaths
  when (null (lefts specs)) $ debugMessage "Finished reading repository"
  return $ (Repository $ rights specs, lefts specs)
 where
  readPackageSpecIO = liftIO readPackageSpec

  readPackageFile f = do
    spec <- readPackageSpecIO $ readCompleteFile f
    return $ case spec of
      Left err -> Left $ "Problem reading '" ++ f ++ "': " ++ err
      Right  s -> Right s

  dirOrSpec d = (not $ isPrefixOf "." d) && takeExtension d /= ".md" &&
                (not $ isPrefixOf repositoryCacheFileName (map toUpper d))

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
      c <- inDirectory (repositoryDir cfg) $
             execQuietCmd $ (\q -> "git pull " ++ q ++ " origin master")
      if c == 0
        then do updateRepositoryCache cfg
                log Info "Successfully updated repository"
        else failIO $ "Failed to update git repository, return code " ++ show c
    else do
      c <- inDirectory (repositoryDir cfg) $ execQuietCmd cloneCommand
      if c == 0
        then do updateRepositoryCache cfg
                log Info "Successfully updated repository"
        else failIO $ "Failed to update git repository, return code " ++ show c
 where
  cloneCommand q = unwords ["git clone", q, packageIndexRepository cfg, "."]


------------------------------------------------------------------------------
-- Operations implementing the repository cache for faster reading.

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
            (packageVersion ++ "\n" ++ showQTerm repo)

--- Reads the given repository from the cache.
readRepositoryCache :: Config -> IO (Maybe Repository)
readRepositoryCache cfg = do
  let cf = repositoryCache cfg
  excache <- doesFileExist cf
  if excache
    then debugMessage ("Reading repository cache from '" ++ cf ++ "'...") >>
         catch (readTermInCacheFile cf)
               (\_ -> do infoMessage "Cleaning broken repository cache..."
                         cleanRepositoryCache cfg
                         return Nothing )
    else return Nothing
 where
  readTermInCacheFile cf = do
    h <- openFile cf ReadMode
    pv <- hGetLine h
    if pv == packageVersion
      then hGetContents h >>= \t -> return $!! Just (readQTerm t)
      else do infoMessage "Cleaning repository cache (wrong version)..."
              cleanRepositoryCache cfg
              return Nothing

--- Cleans the repository cache.
cleanRepositoryCache :: Config -> IO ()
cleanRepositoryCache cfg = do
  let cachefile = repositoryCache cfg
  whenFileExists cachefile $ removeFile cachefile

------------------------------------------------------------------------------
