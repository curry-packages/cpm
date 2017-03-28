--------------------------------------------------------------------------------
--- This module implements functionality surrounding the package *repository*.
--- The repository is the index of all packages known to the package manager. It
--- contains metadata about the packages, such as their names, versions 
--- dependencies and where they can be acquired. The repository does not contain
--- the actual packages. For a list of packages that are currently installed 
--- locally, you can consult the *database*.
--------------------------------------------------------------------------------

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
  ) where

import Char   (toLower)
import Directory
import Either
import List
import FilePath
import System (system)

import CPM.Config (Config, repositoryDir, packageIndexRepository)
import CPM.ErrorLogger
import CPM.Package
import CPM.FileUtil ( checkAndGetDirectoryContents, inDirectory )

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
findAllVersions (Repository ps) p pre = sortedByVersion $ preFiltered $ sameName ps
  where 
    sortedByVersion = sortBy (\a b -> (version a) `vgt` (version b))
    preFiltered = filter filterPre
    sameName = filter ((== p) . name) 
    filterPre p' = pre || (not . isPreRelease . version) p'

--- Search the names and synopses of all packages in the repository for a 
--- particular term. Lower/upercase is ignored for the search.
--- Returns the newest matching version of each package.
---
--- @param repo - the repository
--- @param q - the term to search for
searchPackages :: Repository -> String -> [Package]
searchPackages (Repository ps) searchstring =
  map (head . sortedByVersion) groupedResults
 where
  groupedResults = groupBy namesEqual allResults
  allResults = filter (matches (lowerS searchstring)) ps
  matches q p = q `isInfixOf` (lowerS $ name p) ||
                q `isInfixOf` (lowerS $ synopsis p)
  namesEqual a b = (name a) == (name b)
  sortedByVersion = sortBy (\a b -> (version a) `vgt` (version b))
  lowerS = map toLower

--- Get all packages in the repository and group them by versions
--- (newest first).
---
--- @param repo - the repository
listPackages :: Repository -> [[Package]]
listPackages (Repository ps) =
  map sortedByVersion (groupBy (\a b -> name a == name b) ps)
 where
  sortedByVersion = sortBy (\a b -> (version a) `vgt` (version b))

--- Finds the latest version of a package.
---
--- @param repo - the central package index
--- @param p - the package to search for
--- @param pre - include pre-release versions
findLatestVersion :: Repository -> String -> Bool -> Maybe Package
findLatestVersion repo p pre = case findAllVersions repo p pre of
  [] -> Nothing
  (x:_) -> Just x

--- Finds a specific version of a package.
findVersion :: Repository -> String -> Version -> Maybe Package
findVersion repo p v = maybeHead $ filter ((== v) . version) $ findAllVersions repo p True
  where maybeHead []    = Nothing
        maybeHead (x:_) = Just x

--- Get all packages in the central package index.
allPackages :: Repository -> [Package]
allPackages (Repository ps) = ps

--- Reads all package specifications from the default repository
---
--- @param cfg the configuration to use
readRepository :: Config -> IO (Repository, [String])
readRepository cfg = readRepositoryFrom (repositoryDir cfg)

--- Reads all package specifications from a repository.
---
--- @param path the location of the repository
readRepositoryFrom :: String -> IO (Repository, [String])
readRepositoryFrom path = do
  debugMessage $ "Reading repository index from '" ++ path ++ "'..."
  pkgDirs <- checkAndGetDirectoryContents path
  pkgPaths <- return $ map (path </>) $ filter dirOrSpec pkgDirs
  verDirs <- mapIO checkAndGetDirectoryContents pkgPaths
  verPaths <- return $ concat $ map (\(d, p) -> map (d </>) (filter dirOrSpec p)) $ zip pkgPaths verDirs
  specPaths <- return $ map (</> "package.json") verPaths
  specs <- mapIO readPackageFile specPaths
  when (null (lefts specs)) $ debugMessage "Finished reading repository"
  return $ (Repository $ rights specs, lefts specs)
 where
  readPackageSpecIO = liftIO readPackageSpec

  readPackageFile f = do
    spec <- readPackageSpecIO $ readFile f
    return $ case spec of
      Left err -> Left $ "Problem reading '" ++ f ++ "': " ++ err
      Right  s -> Right s

  dirOrSpec d =  (not $ isPrefixOf "." d) && takeExtension d /= ".md"

--- Updates the package index from the central Git repository.
updateRepository :: Config -> IO (ErrorLogger ())
updateRepository cfg = do
  gitExists <- doesDirectoryExist $ (repositoryDir cfg) </> ".git"
  if gitExists 
    then do
      c <- inDirectory (repositoryDir cfg) $ system $ "git pull origin master" 
      if c == 0
        then log Info "Successfully updated repository"
        else failIO $ "Failed to update git repository, return code " ++ show c
    else do
      c <- inDirectory (repositoryDir cfg) $ system $ cloneCommand
      if c == 0
        then log Info "Successfully updated repository"
        else failIO $ "Failed to update git repository, return code " ++ show c
 where
  cloneCommand = "git clone " ++ packageIndexRepository cfg ++ " ."

