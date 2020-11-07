------------------------------------------------------------------------------
--- This module contains operations implementing a file-based repository cache
--- for faster reading the repository. This file-based implementation
--- is used if the command `sqlite3` is not available (compare module
--- `CPM.RepositoryCache.Init`).
--- The repository cache contains reduced package specifications
--- for faster reading/writing by removing some information
--- which is not relevant for the repository data structure.
---
--- The relevant package fields are:
--- * small cache: name version dependencies compilerCompatibility
--- * large cache: synopsis category sourceDirs exportedModules executableSpec
---
--- @author Michael Hanus
--- @version March 2018
------------------------------------------------------------------------------

module CPM.Repository.CacheFile
  ( readRepository )
 where

import System.Directory ( doesFileExist )
import System.IO
import ReadShowTerm     ( showQTerm, readQTerm, showTerm, readUnqualifiedTerm )

import CPM.Config        ( Config, repositoryDir )
import CPM.ConfigPackage ( packageVersion )
import CPM.ErrorLogger
import CPM.Package
import CPM.Repository

------------------------------------------------------------------------------
--- Reads all package specifications from the default repository.
--- Uses the cache if it is present or update the cache after reading.
--- If some errors occur, show them and terminate with error exit status.
---
--- @param cfg   - the configuration to use
--- @param large - if true reads the larger cache with more package information
---                (e.g., for searching all packages)
readRepository :: Config -> Bool -> ErrorLogger Repository
readRepository cfg large = do
  warnIfRepositoryOld cfg
  mbrepo <- readRepositoryCache cfg large
  case mbrepo of
    Nothing -> do
      repo <- readRepositoryFrom (repositoryDir cfg)
      infoMessage $ "Writing " ++ (if large then "large" else "base") ++
                    " repository cache..."
      liftIOErrorLogger $ writeRepositoryCache cfg large repo
      return repo
    Just repo -> return repo


--- The file containing the repository cache as a Curry term.
repositoryCache :: Config -> Bool -> String
repositoryCache cfg large =
  repositoryCacheFilePrefix cfg ++ (if large then "_LARGE" else "_SMALL")

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
readRepositoryCache :: Config -> Bool -> ErrorLogger (Maybe Repository)
readRepositoryCache cfg large = do
  let cf = repositoryCache cfg large
  excache <- liftIOErrorLogger $ doesFileExist cf
  if excache
    then do debugMessage ("Reading repository cache from '" ++ cf ++ "'...")
            ((if large
                  then readTermInCacheFile cfg (largetuple2package . uread) cf
                  else readTermInCacheFile cfg (smalltuple2package . uread) cf)
                  >>= \repo ->
                debugMessage "Finished reading repository cache" >> return repo)
              <|>
               (do infoMessage "Cleaning broken repository cache..."
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
                    -> ErrorLogger (Maybe Repository)
readTermInCacheFile cfg trans cf = do
  h <- liftIOErrorLogger $ openFile cf ReadMode
  pv <- liftIOErrorLogger $ hGetLine h
  if pv == repoCacheVersion
    then liftIOErrorLogger (hGetContents h) >>= \t ->
         return $!! Just (pkgsToRepository (map trans (lines  t)))
    else do infoMessage "Cleaning repository cache (wrong version)..."
            cleanRepositoryCache cfg
            return Nothing

------------------------------------------------------------------------------
