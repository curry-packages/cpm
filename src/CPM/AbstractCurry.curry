--------------------------------------------------------------------------------
--- This module contains helper functions for dealing with AbstractCurry. In
--- particular, it contains functions that can read modules from a package and
--- its dependencies with all dependencies available to the Curry frontend.
--------------------------------------------------------------------------------

module CPM.AbstractCurry 
  ( loadPathForPackage
  , readAbstractCurryFromPackagePath
  , readAbstractCurryFromDeps
  , transformAbstractCurryInDeps 
  , applyModuleRenames
  , tcArgsOfType
  ) where

import Distribution (FrontendTarget (..), FrontendParams (..), defaultParams
                    , callFrontendWithParams, setQuiet, setFullPath
                    , sysLibPath, inCurrySubdir, modNameToPath
                    , inCurrySubdirModule, lookupModuleSource)
import List (intercalate, nub)
import FilePath ((</>), (<.>), takeFileName, replaceExtension)
import AbstractCurry.Files (readAbstractCurryFile, writeAbstractCurryFile)
import AbstractCurry.Pretty (showCProg)
import AbstractCurry.Select (imports)
import AbstractCurry.Transform
import AbstractCurry.Types
import System

import CPM.ErrorLogger
import qualified CPM.PackageCache.Runtime as RuntimeCache
import CPM.Package (Package, loadPackageSpec, sourceDirsOf)

--- Returns the load path for a package stored in some directory
--- w.r.t. the dependent packages.
---
--- @param - pkg - the package
--- @param - pkgDir - the directory containing this package
--- @param - deps - the resolved dependencies of the package
--- @return the full load path for modules in the package or dependent packages
loadPathForPackage :: Package -> String -> [Package] -> [String]
loadPathForPackage pkg pkgDir deps =
  (map (pkgDir </>) (sourceDirsOf pkg) ++
  RuntimeCache.dependencyPathsSeparate deps pkgDir)

--- Returns the full load path for a package stored in some directory.
---
--- @param - pkg - the package
--- @param - pkgDir - the directory containing this package
--- @param - deps - the resolved dependencies of the package
--- @return the full load path for modules in the package or dependent packages
fullLoadPathForPackage :: Package -> String -> [Package] -> [String]
fullLoadPathForPackage pkg pkgDir deps =
  loadPathForPackage pkg pkgDir deps ++ sysLibPath
  -- here we assume that the system libs are identical for each Curry system

--- Reads an AbstractCurry module from a package.
---
--- @param - dir the package's directory
--- @param - deps the resolved dependencies of the package
--- @param - mod the module to read
readAbstractCurryFromPackagePath :: Package -> String -> [Package] -> String
                                 -> IO CurryProg
readAbstractCurryFromPackagePath pkg pkgDir deps modname = do
  let loadPath = fullLoadPathForPackage pkg pkgDir deps
  params <- return $ setQuiet True (setFullPath loadPath defaultParams)
  callFrontendWithParams ACY params modname 
  src <- lookupModuleSource loadPath modname
  acyName <- return $ case src of
    Nothing -> error $ "Module not found: " ++ modname
    Just (_, file) -> replaceExtension (inCurrySubdirModule modname file) ".acy"
  readAbstractCurryFile acyName

--- Reads an AbstractCurry module from a package or one of its dependencies.
---
--- @param dir - the package's directory
--- @param deps - the resolved dependencies of the package
--- @param mod - the module to read
readAbstractCurryFromDeps :: String -> [Package] -> String -> IO CurryProg
readAbstractCurryFromDeps pkgDir deps modname = do
  pkg <- fromErrorLogger (loadPackageSpec pkgDir)
  let loadPath = fullLoadPathForPackage pkg pkgDir deps
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
  pkg <- fromErrorLogger (loadPackageSpec pkgDir)
  let loadPath = fullLoadPathForPackage pkg pkgDir deps
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
  updCProg maybeRename (map maybeRename) id id id id id id
           (updQNamesInCProg rnm prog)
 where
  maybeRename n = case lookup n names of
    Just n' -> n'
    Nothing -> n
  rnm mn@(mod, n) = case lookup mod names of
    Just mod' -> (mod', n)
    Nothing   -> mn


--- Checks whether a type expression is a type constructor application.
--- If this is the case, return the type constructor and the type arguments.
tcArgsOfType :: CTypeExpr -> Maybe (QName,[CTypeExpr])
tcArgsOfType texp =
  maybe Nothing
        (\tc -> Just (tc, targsOfApply texp))
        (tconOfApply texp)
 where
  tconOfApply te = case te of CTApply (CTCons qn) _ -> Just qn
                              CTApply tc _          -> tconOfApply tc
                              _                     -> Nothing
                                 
  targsOfApply te = case te of
    CTApply (CTCons _) ta -> [ta]
    CTApply tc         ta -> targsOfApply tc ++ [ta]
    _                     -> [] -- should not occur
