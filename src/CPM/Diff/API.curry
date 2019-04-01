--------------------------------------------------------------------------------
--- This module contains functions that can compare the API of one version of a
--- package to another.
--------------------------------------------------------------------------------

module CPM.Diff.API
  ( compareModulesFromPackages
  , compareModulesFromPackageAndDir
  , compareModulesInDirs
  , compareApiModule
  , getBaseTemp
  , Differences
  , Difference (..)
  , showDifferences
  ) where

import AbstractCurry.Types (CurryProg (..), CFuncDecl (..), CTypeDecl (..)
                           , COpDecl (..), QName, CFixity (..)
                           , CVisibility (..))
import AbstractCurry.Pretty
import AbstractCurry.Select (functions, funcName, types, typeName)
import System.Directory     (getTemporaryDirectory)
import System.FilePath      ((</>))
import Data.List            (nub)
import Data.Maybe           (listToMaybe, catMaybes)
import Prelude hiding (empty, log)

import Text.Pretty (pPrint, text, (<+>), vcat, empty, red, ($$))

import CPM.AbstractCurry (readAbstractCurryFromPackagePath)
import CPM.Config (Config)
import CPM.ErrorLogger
import CPM.FileUtil (copyDirectory, recreateDirectory)
import CPM.Package (Package, Version, packageId, loadPackageSpec
                   , exportedModules)
import CPM.PackageCache.Global as GC
import CPM.PackageCopy (resolveAndCopyDependencies)
import CPM.Repository (Repository)

getBaseTemp :: IO String
getBaseTemp = getTemporaryDirectory >>=
  \tmpDir -> let tmp = tmpDir </> "cpm" </> "diff"
              in recreateDirectory tmp >> return tmp

--- Compares two versions of a package from the global package cache.
---
--- @param cfg - the CPM configuration
--- @param repo - the central package index
--- @param gc - the global package cache
--- @param nameA - the name of package version A
--- @param verA - the version of package version A
--- @param nameB - the name of package version B
--- @param verB - the version of package version B
--- @param onlyMods - a list of modules to compare
compareModulesFromPackages :: Config -> Repository -> GC.GlobalCache -> String
                           -> Version -> String -> Version -> Maybe [String]
                           -> ErrorLogger [(String, Differences)]
compareModulesFromPackages cfg repo gc nameA verA nameB verB onlyMods = do
  baseTmp <- liftIOErrorLogger getBaseTemp
  pkgA <- GC.tryFindPackage gc nameA verA
  pkgB <- GC.tryFindPackage gc nameB verB
  GC.copyPackage cfg pkgA baseTmp
  GC.copyPackage cfg pkgB baseTmp
  compareModulesInDirs cfg repo gc (baseTmp </> packageId pkgA)
    (baseTmp </> packageId pkgB) onlyMods

--- Compares a package version from a directory to a package version from the
--- global package cache.
---
--- @param cfg - the CPM configuration
--- @param repo - the central package index
--- @param gc - the global package cache
--- @param dirA - the directory containing package version A
--- @param nameB - the name of package version B
--- @param verB - the version of package version B
--- @param onlyMods - a list of modules to compare
compareModulesFromPackageAndDir :: Config -> Repository -> GC.GlobalCache
                               -> String -> String -> Version -> Maybe [String]
                               -> ErrorLogger [(String, Differences)]
compareModulesFromPackageAndDir cfg repo gc dirA nameB verB onlyMods = do
  baseTmp <- liftIOErrorLogger getBaseTemp
  pkgB <- GC.tryFindPackage gc nameB verB
  pkgA <- loadPackageSpec dirA
  GC.copyPackage cfg pkgB baseTmp
  liftIOErrorLogger $ copyDirectory dirA (baseTmp </> packageId pkgA)
  compareModulesInDirs cfg repo gc (baseTmp </> packageId pkgA)
                       (baseTmp </> packageId pkgB) onlyMods

--- Compares package versions from two directories.
---
--- @param cfg - the CPM configuration
--- @param repo - the central package index
--- @param gc - the global package cache
--- @param dirA - the directory containing package version A
--- @param dirB - the directory containing package version B
--- @param onlyMods - a list of modules to compare
compareModulesInDirs :: Config -> Repository -> GC.GlobalCache -> String
                     -> String -> Maybe [String]
                     -> ErrorLogger [(String, Differences)]
compareModulesInDirs cfg repo gc dirA dirB onlyMods =
  loadPackageSpec dirA >>= \pkgA ->
  loadPackageSpec dirB >>= \pkgB ->
  resolveAndCopyDependencies cfg repo gc dirA >>= \depsA ->
  resolveAndCopyDependencies cfg repo gc dirB >>= \depsB ->
  let cmpmods = nub (exportedModules pkgA ++ exportedModules pkgB) in
  if null cmpmods
    then log Info "No exported modules to compare" >> return []
    else do diffs <- liftIOErrorLogger $ mapM (compareApiModule
                       pkgA dirA depsA pkgB dirB depsB) cmpmods
            let modsWithDiffs = zip cmpmods diffs
            return $ case onlyMods of
              Nothing -> modsWithDiffs
              Just ms -> filter ((`elem` ms) . fst) modsWithDiffs

--- Compares a single module from two package versions.
---
--- @param pkgA - version A of the package
--- @param dirA - the directory containing version A of the package
--- @param depsA - the resolved dependencies of version A of the package
--- @param pkgB - version B of the package
--- @param dirB - the directory containing version B of the package
--- @param depsB - the resolved dependencies of version B of the package
--- @param mod - the name of the module
compareApiModule :: Package -> String -> [Package] -> Package -> String
                 -> [Package] -> String -> IO Differences
compareApiModule pkgA dirA depsA pkgB dirB depsB mod =
  if mod `elem` exportedModules pkgA
    then
     if mod `elem` exportedModules pkgB
       then
         readAbstractCurryFromPackagePath pkgA dirA depsA mod
              >>= \prog1 ->
         readAbstractCurryFromPackagePath pkgB dirB depsB mod
              >>= \prog2 ->
         let funcDiffs = diffFuncsFiltered funcIsPublic   prog1 prog2
             typeDiffs = diffTypesFiltered typeIsPublic   prog1 prog2
             opDiffs   = diffOpsFiltered   (\_ _ -> True) prog1 prog2
         in return $ (Nothing, funcDiffs, typeDiffs, opDiffs)
      else return $ (Just $ Addition mod, [], [], [])
    else return $ (Just $ Removal mod, [], [], [])

--- Differences between two versions of a package. First component is present
--- if the module is missing in one of the package versions. The other
--- components list the differences if the module is present in both versions.
type Differences = ( Maybe (Difference String)
                   , [Difference CFuncDecl]
                   , [Difference CTypeDecl]
                   , [Difference COpDecl]
                   )

--- A single difference between two versions of a module.
data Difference a = Addition a
                  | Removal a
                  | Change a a

--- Prints a list of differences to the user.
---
--- @param diffs - the list of differences
--- @param verA - version A of the package
--- @param verB - version B of the package
showDifferences :: [Differences] -> Version -> Version -> String
showDifferences diffs verA verB = pPrint $
  vcat (map showDifferences' diffs)
 where
  jump = versionJump verA verB
  showDifferences' (modDiff, funcDiffs, typeDiffs, opDiffs) =
    (modText modDiff)
    $$ (vcat $ funcTexts funcDiffs)
    $$ (vcat $ typeTexts typeDiffs)
    $$ (vcat $ opTexts opDiffs)
  showViolation (Addition _) = if jump == Patch
    then red $ text "Adding features in a patch version is a violation of semantic versioning."
    else empty
  showViolation (Removal _) = if jump /= Major
    then red $ text "Removing features in a patch or minor version is a violation of semantic versioning."
    else empty
  showViolation (Change _ _) = if jump /= Major
    then red $ text "Changing APIs in a patch or minor version is a violation of semantic versioning."
    else empty
  funcTexts funcDiffs =
    map (\f -> (text $ showFuncDifference f) <+> (showViolation f)) funcDiffs
  typeTexts typeDiffs =
    map (\f -> (text $ showTypeDifference f) <+> (showViolation f)) typeDiffs
  opTexts opDiffs =
    map (\f -> (text $ showOpDifference f) <+> (showViolation f)) opDiffs
  modText modDiff = case modDiff of
    Nothing -> empty
    Just  d -> case d of
      Addition m -> (text $ "Added module " ++ m) <+> (showViolation d)
      Removal  m -> (text $ "Removed module " ++ m) <+> (showViolation d)
      Change _ _ -> text $ "This should not appear"

--- A jump between two versions.
data VersionJump = Major | Minor | Patch | None
 deriving Eq

--- Calculate the jump between two versions.
versionJump :: Version -> Version -> VersionJump
versionJump (majA, minA, patA, _) (majB, minB, patB, _) =
  if majA /= majB
    then Major
    else if minA /= minB
      then Minor
      else if patA /= patB
        then Patch
        else None

--- Renders a function difference to a string.
showFuncDifference :: Difference CFuncDecl -> String
showFuncDifference (Addition f) = "Added " ++ (showFuncDecl f)
showFuncDifference (Removal f)  = "Removed " ++ (showFuncDecl f)
showFuncDifference (Change a b) = "Change " ++ (showFuncDecl a) ++ " to " ++ (showFuncDecl b)

--- Renders a function declaration to a string.
showFuncDecl :: CFuncDecl -> String
showFuncDecl (CFunc (_, n) _ _ t _) =
  n ++ " :: " ++ (pPrint $ ppCQualTypeExpr defaultOptions t)
showFuncDecl (CmtFunc _ (_, n) _ _ t _) =
  n ++ " :: " ++ (pPrint $ ppCQualTypeExpr defaultOptions t)

--- Renders a type difference to a string.
showTypeDifference :: Difference CTypeDecl -> String
showTypeDifference (Addition f) = "Added " ++ (showTypeDecl f)
showTypeDifference (Removal f)  = "Removed " ++ (showTypeDecl f)
showTypeDifference (Change a b) = "Changed " ++ (showTypeDecl a) ++ " to " ++ (showTypeDecl b)

--- Renders a type declaration to a string.
showTypeDecl :: CTypeDecl -> String
showTypeDecl (CType (_, n) _ _ cs _) =
  "data " ++  n ++ " (" ++ (show $ length cs) ++ " constructors)"
showTypeDecl (CTypeSyn (_, n) _ _ t) =
  "type " ++ n ++ " = " ++ (pPrint $ ppCTypeExpr defaultOptions t)
showTypeDecl (CNewType (_, n) _ _ _ _) = "newtype " ++ n

--- Renders an operator difference to a string.
showOpDifference :: Difference COpDecl -> String
showOpDifference (Addition f) = "Added " ++ showOpDecl f
showOpDifference (Removal f)  = "Removed " ++ showOpDecl f
showOpDifference (Change a b) = "Changed " ++ showOpDecl a ++ " to " ++
                                showOpDecl b

--- Renders an operator declaration to a string.
showOpDecl :: COpDecl -> String
showOpDecl (COp (_, n) CInfixOp a) = "infix " ++ (show a) ++ " " ++ n
showOpDecl (COp (_, n) CInfixlOp a) = "infixl " ++ (show a) ++ " " ++ n
showOpDecl (COp (_, n) CInfixrOp a) = "infixr " ++ (show a) ++ " " ++ n

--- Compares all functions in two modules that match the given predicate.
diffFuncsFiltered :: (CurryProg -> CFuncDecl -> Bool) -> CurryProg -> CurryProg
                  -> [Difference CFuncDecl]
diffFuncsFiltered = mkDiff funcEq functions funcName

--- Compares all type declarations in two modules that match the given
--- predicate.
diffTypesFiltered :: (CurryProg -> CTypeDecl -> Bool) -> CurryProg -> CurryProg
                  -> [Difference CTypeDecl]
diffTypesFiltered = mkDiff typeEq types typeName

--- Compares all operator declarations in two modules that match the given
--- predicate.
diffOpsFiltered :: (CurryProg -> COpDecl -> Bool) -> CurryProg -> CurryProg
                -> [Difference COpDecl]
diffOpsFiltered = mkDiff opEq ops opName

--- Is a function public?
funcIsPublic :: CurryProg -> CFuncDecl -> Bool
funcIsPublic _ (CFunc _ _ Public _ _) = True
funcIsPublic _ (CFunc _ _ Private _ _) = False
funcIsPublic _ (CmtFunc _ _ _ Public _ _) = True
funcIsPublic _ (CmtFunc _ _ _ Private _ _) = False

--- Is a type public?
typeIsPublic :: CurryProg -> CTypeDecl -> Bool
typeIsPublic _ (CType _ Public _ _ _) = True
typeIsPublic _ (CType _ Private _ _ _) = False
typeIsPublic _ (CTypeSyn _ Public _ _) = True
typeIsPublic _ (CTypeSyn _ Private _ _) = False
typeIsPublic _ (CNewType _ Public _ _ _) = True
typeIsPublic _ (CNewType _ Private _ _ _) = False

--- Creates a function that can compare elements in two versions of a module.
---
--- @param eq - a function that checks if two elements are equal
--- @param selector - a function that selects the elements to be compared from
---                   a CurryProg
--- @param name - a function that selects the name of an element
mkDiff :: (a -> a -> Bool)
       -> (CurryProg -> [a])
       -> (a -> QName)
       -> ((CurryProg -> a -> Bool) -> CurryProg -> CurryProg -> [Difference a])
mkDiff eq selector name =
  \p b a -> let
    as = filter (p a) $ selector a
    bs = filter (p b) $ selector b
    findDifference f = case listToMaybe $ filter ((== (name f)) . name) bs of
      Nothing -> Just $ Removal f
      Just f' -> if f `eq` f'
        then Nothing
        else Just $ Change f f'
    additions = filter (not . (flip elem) (map name as) . name) bs
  in
    catMaybes (map findDifference as) ++ (map Addition additions)

--- Are two functions equal?
funcEq :: CFuncDecl -> CFuncDecl -> Bool
funcEq (CFunc _ a1 v1 t1 _) (CFunc _ a2 v2 t2 _) = a1 == a2 && v1 == v2 && t1 == t2
funcEq (CmtFunc _ _ a1 v1 t1 _) (CmtFunc _ _ a2 v2 t2 _) = a1 == a2 && v1 == v2 && t1 == t2
funcEq (CFunc _ a1 v1 t1 _) (CmtFunc _ _ a2 v2 t2 _) = a1 == a2 && v1 == v2 && t1 == t2
funcEq (CmtFunc _ _ a1 v1 t1 _) (CFunc _ a2 v2 t2 _) = a1 == a2 && v1 == v2 && t1 == t2

--- Are two type declarations equal? (We ignore `deriving` clauses)
typeEq :: CTypeDecl -> CTypeDecl -> Bool
typeEq (CType _ v1 tvs1 cs1 _) (CType _ v2 tvs2 cs2 _) =
  v1 == v2 && tvs1 == tvs2 && cs1 == cs2
typeEq (CTypeSyn _ v1 tvs1 e1) (CTypeSyn _ v2 tvs2 e2) =
  v1 == v2 && tvs1 == tvs2 && e1 == e2
typeEq (CNewType _ v1 tvs1 c1 _) (CNewType _ v2 tvs2 c2 _) =
  v1 == v2 && tvs1 == tvs2 && c1 == c2
typeEq (CType _ _ _ _ _) (CTypeSyn _ _ _ _) = False
typeEq (CType _ _ _ _ _) (CNewType _ _ _ _ _) = False
typeEq (CTypeSyn _ _ _ _) (CType _ _ _ _ _) = False
typeEq (CTypeSyn _ _ _ _) (CNewType _ _ _ _ _) = False
typeEq (CNewType _ _ _ _ _) (CType _ _ _ _ _) = False
typeEq (CNewType _ _ _ _ _) (CTypeSyn _ _ _ _) = False

--- Are two operator declarations equal?
opEq :: COpDecl -> COpDecl -> Bool
opEq (COp _ f1 a1) (COp _ f2 a2) = f1 == f2 && a1 == a2

--- Select all operator declarations from a CurryProg.
ops :: CurryProg -> [COpDecl]
ops (CurryProg _ _ _ _ _ _ _ os) = os

--- Get the name of an operator declaration.
opName :: COpDecl -> QName
opName (COp n _ _) = n
