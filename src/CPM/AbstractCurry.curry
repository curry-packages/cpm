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
  ) where

import List         ( intercalate, nub )
import FilePath     ( (</>), (<.>), takeFileName, replaceExtension )
import System

import AbstractCurry.Files  ( readAbstractCurryFile, writeAbstractCurryFile )
import AbstractCurry.Pretty ( showCProg )
import AbstractCurry.Select ( imports )
import AbstractCurry.Transform
import AbstractCurry.Types
import System.CurryPath     ( sysLibPath, inCurrySubdir, modNameToPath
                            , inCurrySubdirModule, lookupModuleSource )
import System.FrontendExec  ( FrontendTarget (..), FrontendParams (..)
                            , defaultParams, callFrontendWithParams
                            , setQuiet, setFullPath )

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
  readAbstractCurryFile acyName >>= return . addPrimTypes
 where
  -- work-around for missing Prelude.Char|Int|Float declarations:
  addPrimTypes p@(CurryProg mname imports dfltdecl clsdecls instdecls
                            typedecls funcdecls opdecls)
   | mname == pre && primType "Int" `notElem` typedecls
   = CurryProg mname imports dfltdecl clsdecls instdecls
               (map primType ["Int","Float","Char"] ++ typedecls)
               funcdecls opdecls
   | otherwise = p
   where pre = "Prelude"
         primType n = CType ("Prelude",n) Public [] [] []

--- Reads an AbstractCurry module from a package or one of its dependencies.
---
--- @param dir - the package's directory
--- @param deps - the resolved dependencies of the package
--- @param mod - the module to read
readAbstractCurryFromDeps :: String -> [Package] -> String -> IO CurryProg
readAbstractCurryFromDeps pkgDir deps modname = do
  pkg <- fromErrorLogger (loadPackageSpec pkgDir)
  readAbstractCurryFromPackagePath pkg pkgDir deps modname

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
  acy <- readAbstractCurryFromDeps pkgDir deps modname
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

