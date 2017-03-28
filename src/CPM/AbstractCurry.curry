--------------------------------------------------------------------------------
--- This module contains helper functions for dealing with AbstractCurry. In
--- particular, it contains functions that can read modules from a package and
--- its dependencies with all dependencies available to the Curry frontend.
--------------------------------------------------------------------------------

module CPM.AbstractCurry 
  ( loadPathForPackage
  , readAbstractCurryFromPath
  , readAbstractCurryFromDeps
  , transformAbstractCurryInDeps 
  , applyModuleRenames
  ) where

import Distribution (FrontendTarget (..), FrontendParams (..), defaultParams
                    , callFrontendWithParams, setQuiet, setFullPath, sysLibPath
                    , curryCompiler, installDir, inCurrySubdir, modNameToPath
                    , inCurrySubdirModule, lookupModuleSource)
import List (intercalate, nub)
import FilePath ((</>), (<.>), takeFileName, replaceExtension)
import AbstractCurry.Files (readAbstractCurryFile, writeAbstractCurryFile)
import AbstractCurry.Pretty (showCProg)
import AbstractCurry.Select (imports)
import AbstractCurry.Transform
import AbstractCurry.Types (CurryProg)
import System

import qualified CPM.PackageCache.Runtime as RuntimeCache
import CPM.Package (Package)

--- Returns the load path for a package stored in some directory
--- w.r.t. the dependent packages
---
--- @param - pkgDir - the package's directory
--- @param - deps - the resolved dependencies of the package
--- @return the full load path for modules in the package or dependent packages
loadPathForPackage :: String -> [Package] -> [String]
loadPathForPackage pkgDir deps =
  [pkgDir </> "src"] ++  RuntimeCache.dependencyPathsSeparate deps pkgDir
  
--- Returns the full load path for a package stored in some directory.
---
--- @param - pkgDir - the package's directory
--- @param - deps - the resolved dependencies of the package
--- @return the full load path for modules in the package or dependent packages
fullLoadPathForPackage :: String -> [Package] -> [String]
fullLoadPathForPackage pkgDir deps =
  sysLibPath ++ loadPathForPackage pkgDir deps
  
--- Reads an AbstractCurry module from a package.
---
--- @param - dir the package's directory
--- @param - deps the resolved dependencies of the package
--- @param - mod the module to read
readAbstractCurryFromPath :: String -> [Package] -> String -> IO CurryProg
readAbstractCurryFromPath pkgDir deps modname = do
  let loadPath = fullLoadPathForPackage pkgDir deps
  params <- return $ setQuiet True (setFullPath loadPath defaultParams)
  callFrontendWithParams ACY params modname 
  readAbstractCurryFile acyName
 where
  acyName = inCurrySubdir (pkgDir </> "src" </> modname) ++ ".acy"
  
--- Reads an AbstractCurry module from a package or one of its dependencies.
---
--- @param dir - the package's directory
--- @param deps - the resolved dependencies of the package
--- @param mod - the module to read
readAbstractCurryFromDeps :: String -> [Package] -> String -> IO CurryProg
readAbstractCurryFromDeps pkgDir deps modname = do
  let loadPath = fullLoadPathForPackage pkgDir deps
  params <- return $ setQuiet True (setFullPath loadPath defaultParams)
  src <- lookupModuleSource loadPath modname
  sourceFile <- return $ case src of
    Nothing -> error $ "Module not found: " ++ modname
    Just (_, file) -> replaceExtension (inCurrySubdirModule modname file) ".acy"
  callFrontendWithParams ACY params modname
  readAbstractCurryFile sourceFile

--- Applies a transformation function to a module from a package or one of its
--- dependencies and writes the modified module to a file in Curry form.
---
--- @param dir - the package's directory
--- @param deps - the resolved dependencies of the package
--- @param f - the transformation function
--- @param mod - the module to transform
--- @param dest - the destination file for the transformed module
transformAbstractCurryInDeps :: String -> [Package] -> (CurryProg -> CurryProg) 
                             -> String -> String -> IO ()
transformAbstractCurryInDeps pkgDir deps transform modname destFile = do
  let loadPath = fullLoadPathForPackage pkgDir deps
  params <- return $ setQuiet True (setFullPath loadPath defaultParams)
  src <- lookupModuleSource loadPath modname
  sourceFile <- return $ case src of
    Nothing -> error $ "Module not found: " ++ modname
    Just (_, file) -> replaceExtension (inCurrySubdirModule modname file) ".acy"
  callFrontendWithParams ACY params modname
  acy <- readAbstractCurryFile sourceFile
  writeFile destFile $ showCProg (transform acy)

--- Renames all references to some modules in a Curry program.
--- 
--- @param mods - a map from old to new module names
--- @param prog - the program to modify
applyModuleRenames :: [(String, String)] -> CurryProg -> CurryProg
applyModuleRenames names prog =
  updCProg maybeRename (map maybeRename) id id id (updQNamesInCProg rnm prog)
 where
  maybeRename n = case lookup n names of
    Just n' -> n'
    Nothing -> n
  rnm mn@(mod, n) = case lookup mod names of
    Just mod' -> (mod', n)
    Nothing   -> mn

