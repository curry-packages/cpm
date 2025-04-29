------------------------------------------------------------------------------
--- This module implements the LookupSet datatype. A lookup set is used to store
--- and query packages for dependency resolution. It stores the source of a
--- package specification alongside the specification itself (e.g. the global
--- repository or the local package cache).
------------------------------------------------------------------------------

module CPM.LookupSet
  ( LookupSource (..)
  , LookupSet
  , emptySet
  , addPackage
  , findLatestVersion
  , findAllVersions
  , findVersion
  , addPackages
  , allPackages
  , lookupSource
  , setLocallyIgnored
  ) where

import Data.List (sortBy, delete, deleteBy)
import Test.Prop
import Prelude hiding (empty)

import Data.Map as Map ( Map, empty, lookup, toList, insert )

import CPM.Package

------------------------------------------------------------------------------

data LookupSource = FromRepository
                  | FromLocalCache
                  | FromGlobalCache

type PkgMap = Map.Map String [(LookupSource, Package)]

data LookupSet = LookupSet PkgMap LookupOptions

data LookupOptions = LookupOptions
  { ignoreLocalVersions :: [String] }

--- The empty lookup set.
emptySet :: LookupSet
emptySet = LookupSet Map.empty defaultOptions

defaultOptions :: LookupOptions
defaultOptions = LookupOptions []

--- Set the set of packages whose locally installed versions are ignored when
--- finding all package versions.
setLocallyIgnored :: LookupSet -> [String] -> LookupSet
setLocallyIgnored (LookupSet ls o) pkgs =
  LookupSet ls (o { ignoreLocalVersions = pkgs })

--- Adds multiple packages to a lookup set with the same source.
---
--- @param l the set to add to
--- @param p the packages to add
--- @param s where are the package specs from?
addPackages :: LookupSet -> [Package] -> LookupSource -> LookupSet
addPackages ls pkgs src = foldl (\l p -> addPackage l p src) ls pkgs

allPackages :: LookupSet -> [Package]
allPackages (LookupSet ls _) = map snd $ concat $ map snd $ toList ls

--- Adds a package to a lookup set.
---
--- @param l the set to add to
--- @param p the package to add
--- @param s where is the package spec from?
addPackage :: LookupSet -> Package -> LookupSource -> LookupSet
addPackage (LookupSet ls o) pkg src = case Map.lookup (name pkg) ls of
  Nothing -> LookupSet (insert (name pkg) [(src, pkg)] ls) o
  Just ps -> let ps' = filter ((/= packageId pkg) . packageId . snd) ps
              in LookupSet (insert (name pkg) ((src, pkg):ps') ls) o

--- Finds a specific entry (including the source) in the lookup set.
---
--- @param l the lookup set
--- @param p the package to search for
findEntry :: LookupSet -> Package -> Maybe (LookupSource, Package)
findEntry (LookupSet ls _) p = maybeHead candidates
 where
  allVersions = Map.lookup (name p) ls
  candidates = case allVersions of
    Nothing -> []
    Just ps -> filter ((packageIdEq p) . snd) ps

--- Finds all versions of a package known to the lookup set. Returns the
--- packages from the local cache first, and then from other sources. Each
--- group is sorted from newest do oldest version.
---
--- @param l the lookup set
--- @param p the name of the package to search for
--- @param pre should pre-release versions be included?
findAllVersions :: LookupSet -> String -> Bool -> [Package]
findAllVersions (LookupSet ls o) p pre = localSorted' ++ nonLocalSorted
  where
    packageVersions = case Map.lookup p ls of
      Nothing -> []
      Just vs -> vs
    onlyLocal = filter isLocal packageVersions
    onlyNonLocal = filter (not . isLocal) packageVersions
    localSorted = sortedByVersion $ preFiltered $ sameName $ ps $ onlyLocal
    localSorted' = filter (not . (flip elem) (ignoreLocalVersions o) . name) localSorted
    nonLocalSorted = sortedByVersion $ preFiltered $ sameName $ ps $ onlyNonLocal
    sortedByVersion = sortBy (\a b -> (version a) `vgt` (version b))
    preFiltered = filter filterPre
    sameName = filter ((== p) . name)
    filterPre p' = pre || (not . isPreRelease . version) p'
    isLocal (FromLocalCache, _) = True
    isLocal (FromGlobalCache, _) = False
    isLocal (FromRepository, _) = False
    ps = map snd

test_findAllVersions_localBeforeNonLocal :: Prop
test_findAllVersions_localBeforeNonLocal = findAllVersions ls "A" False -=- [aLocal, aNonLocal]
  where aLocal = cPackage "A" (1, 0, 0, Nothing) []
        aNonLocal = cPackage "A" (1, 1, 0, Nothing) []
        ls = addPackage (addPackage emptySet aLocal FromLocalCache) aNonLocal FromRepository

test_findAllVersions_nonLocalIfIgnored :: Prop
test_findAllVersions_nonLocalIfIgnored = findAllVersions ls "A" False -=- [aNonLocal]
  where aLocal = cPackage "A" (1, 0, 0, Nothing) []
        aNonLocal = cPackage "A" (1, 1, 0, Nothing) []
        ls = setLocallyIgnored (addPackage (addPackage emptySet aLocal FromLocalCache) aNonLocal FromRepository) ["A"]

cPackage :: String -> Version -> [Dependency] -> Package
cPackage p v ds = emptyPackage {
    name = p
  , version = v
  , author = ["author"]
  , synopsis = "JSON library for Curry"
  , dependencies = ds
  , maintainer = []
  , description = Nothing
  , license = Nothing
  , licenseFile = Nothing
  , copyright = Nothing
  , homepage = Nothing
  , bugReports = Nothing
  , repository = Nothing
  , compilerCompatibility = []
  , source = Nothing
  , exportedModules = []
  }

cDB :: [Package] -> LookupSet
cDB ps = addPackages emptySet ps FromRepository

--- Finds the source for a package in the lookup set
---
--- @param ls the lookup set
--- @param p the package to search for
lookupSource :: LookupSet -> Package -> Maybe LookupSource
lookupSource ls p = case findEntry ls p of
  Nothing     -> Nothing
  Just (s, _) -> Just s

--- Finds the latest version of a package known to the lookup set.
---
--- @param l the lookup set
--- @param p the name of the package to search for
--- @param pre should pre-release versions be included?
findLatestVersion :: LookupSet -> String -> Bool -> Maybe Package
findLatestVersion ls p pre = case findAllVersions ls p pre of
  [] -> Nothing
  (x:_) -> Just x

--- Finds a specific version of a package in the lookup set.
---
--- @param l the lookup set
--- @param p the name of the package
--- @param v the package version
findVersion :: LookupSet -> String -> Version -> Maybe Package
findVersion ls p v =
  maybeHead $ filter ((== v) . version) $ findAllVersions ls p True

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x
