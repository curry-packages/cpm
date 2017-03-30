--------------------------------------------------------------------------------
--- Contains a function that prefixes all modules in a package and all modules
--- in all of its transitive dependencies with a given string.
--------------------------------------------------------------------------------

module CPM.Diff.Rename (prefixPackageAndDeps) where

import Directory (doesDirectoryExist, getDirectoryContents, createDirectory)
import FilePath ((</>), joinPath, takeDirectory, takeBaseName, takeExtension)
import List (splitOn)

import CPM.AbstractCurry (transformAbstractCurryInDeps, applyModuleRenames)
import CPM.Config (Config)
import CPM.ErrorLogger
import CPM.Package (Package, loadPackageSpec)
import CPM.PackageCache.Runtime as RuntimeCache
import CPM.PackageCache.Global as GC
import CPM.PackageCopy (resolveAndCopyDependencies)
import CPM.Repository (Repository)

-- 1. Find all transitive package dependencies.
-- 2. Collect all Curry modules in all dependencies in a list with module name
--    and actual path on disk.
-- 3. Build a map from old module names to prefixed module names.
-- 4. Copy each Curry module in the list to its new, prefixed location. If the
--    module name contains a dot, then the top-level folder gets the prefix. If
--    it contains no dot, then the Curry file itself gets the prefix. Transform
--    the Curry module while copying it.

--- Prefix all modules in a package and all modules in all of its transitive
--- dependencies with a string. 
--- 
--- @param cfg - the CPM configuration
--- @param repo - the central package index
--- @param gc - the global package cache
--- @param dir - the directory of the package
--- @param prefix - the prefix for all module names
--- @param destDir - the destination directory for the modified modules
prefixPackageAndDeps :: Config -> Repository -> GC.GlobalCache -> String 
                     -> String -> String -> IO (ErrorLogger [(String, String)])
prefixPackageAndDeps cfg repo gc dir prefix destDir = 
  resolveAndCopyDependencies cfg repo gc dir |>= 
  \deps -> (mapIO (findAllModulesInPackage . RuntimeCache.cacheDirectory dir) deps >>= succeedIO) |>=
  \depMods -> (findAllModulesInPackage dir >>= succeedIO) |>=
  \ownMods -> succeedIO (ownMods ++ concat depMods) |>=
  \allMods -> succeedIO (zip (map fst allMods) (map ((prefix ++) . fst) allMods)) |>=
  \modMap  -> mapIO (copyMod dir deps destDir modMap) allMods >>
  succeedIO modMap

--- Finds all modules in a package.
findAllModulesInPackage :: String -> IO [(String, String)]
findAllModulesInPackage dir = findMods "" (dir </> "src")
 where
  findMods p d = do
    entries <- getDirectoryContents d
    filteredEntries <- return $ filter (\r -> length r >= 1 && head r /= '.') entries
    curryFiles <- return $ filter ((== ".curry") . takeExtension) filteredEntries
    directoryFlags <- mapIO doesDirectoryExist (map (d </>) filteredEntries)
    directories <- return $ map fst $ filter snd $ zip filteredEntries directoryFlags
    depMods <- mapIO (\d' -> findMods d' (d </> d')) directories
    return $ (map (modWithPath p d) curryFiles) ++ concat depMods
  modWithPath p d m = if p == "" then (takeBaseName m, d </> m)
                                 else (p ++ "." ++ takeBaseName m, d </> m)

--- Copies a module from one directory to another while renaming both the module
--- itself as well as any references to other modules inside that module.
copyMod :: String -> [Package] -> String -> [(String, String)] 
        -> (String, String) -> IO ()
copyMod origDir deps dest nameMap (name, _) = do
  dirExists <- doesDirectoryExist (takeDirectory destPath)
  if dirExists
    then return ()
    else createDirectory (takeDirectory destPath)
  transformAbstractCurryInDeps origDir deps (applyModuleRenames nameMap)
                               name destPath
 where
  newName = case lookup name nameMap of
    Nothing -> name
    Just n' -> n'
  pathParts = splitOn "." newName
  destPath = (joinPath (dest:pathParts)) ++ ".curry"

