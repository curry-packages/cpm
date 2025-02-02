-------------------------------------------------------------------------------
--- This module contains functions that compare the behavior of two versions of
--- a package.
---
--- For this purpose, copies of these packages and a main "comparison"
--- module (with name `Compare`) are generated in the temporary
--- directory `/tmp/CPM/bdiff` and then CurryCheck is executed on `Compare`.
--------------------------------------------------------------------------------

module CPM.Diff.Behavior
  ( ComparisonInfo (..)
  , getBaseTemp
  , genCurryCheckProgram
  , diffBehavior
  , preparePackageDirs
  , preparePackageAndDir
  , preparePackages
  , findFunctionsToCompare
  ) where

import Control.Monad
import Data.Char            ( isAlphaNum )
import Data.List            ( intercalate, intersect, nub, splitOn, isPrefixOf
                            , isInfixOf, find, delete, (\\), nubBy )
import Data.Maybe           ( isJust, fromJust, fromMaybe, listToMaybe )
import System.Environment   ( getEnv, setEnv, unsetEnv )

import AbstractCurry.Build
import AbstractCurry.Pretty ( defaultOptions, ppCTypeExpr, showCProg )
import AbstractCurry.Select ( publicFuncNames, funcName, functions, funcArity
                            , funcType, argTypes, typeName, types, tconsOfType
                            , tconsArgsOfType, resultType, isIOType
                            , typeOfQualType )
import AbstractCurry.Transform (updCFuncDecl)
import AbstractCurry.Types ( CurryProg (..), CFuncDecl (..), CVisibility (..)
                           , CTypeExpr (..), CPattern (..), CExpr (..)
                           , CTypeDecl (..), CConsDecl (..), CFieldDecl (..)
                           , CVarIName, QName)
import Analysis.Types       ( Analysis )
import Analysis.ProgInfo    ( ProgInfo, emptyProgInfo, combineProgInfo
                            , lookupProgInfo)
import Analysis.Termination ( productivityAnalysis, Productivity(..) )
import Analysis.TypeUsage   ( typesInValuesAnalysis )
import CASS.Server          ( analyzeGeneric )
import RW.Base              ( ReadWrite )
import System.CurryPath     ( lookupModuleSource )
import System.Directory     ( createDirectory, doesDirectoryExist
                            , getTemporaryDirectory )
import System.FilePath      ( (</>), joinPath )
import System.Path          ( getFileInPath )
import Text.Pretty          ( pPrint, text, indent, vcat, (<+>), (<$$>) )

import CPM.AbstractCurry ( readAbstractCurryFromDeps, loadPathForPackage )
import CPM.Config        ( Config (curryExec) )
import CPM.Diff.API as APIDiff
import CPM.Diff.CurryComments (readComments, getFuncComment)
import CPM.Diff.Rename (prefixPackageAndDeps)
import CPM.ErrorLogger
import CPM.FileUtil ( copyDirectory, recreateDirectory, inDirectory
                    , joinSearchPath, tempDir )
import CPM.Package ( Package, Version, name, version, showVersion, packageId
                   , exportedModules, loadPackageSpec)
import CPM.PackageCache.Global as GC
import CPM.PackageCopy (resolveAndCopyDependencies)
import CPM.Repository (Repository)

-- What this module does (and how)
-- ===============================
--
-- This module compares two package versions using CurryCheck/EasyCheck. Each
-- function that can be tested (the criteria for what makes a function testable
-- are listed below), is compared using a EasyCheck property test equating both
-- versions of the function. A function is considered testable, if
--
-- - it is present in both versions of the module AND
-- - its type is unchanged between both versions of the module AND
-- - it is public AND
-- - its argument types are either all types from the Curry standard library or
--   they are the same in both versions of the module (including types in
--   package dependencies) AND
-- - the function is not marked with a do-not-checked pragma
--
-- To test a function, we have to generate a new Curry program containing a test
-- that calls both versions of the function (from the old and from the new
-- version of the package) and compares the results. Since we have to use both
-- versions of the package from within the same Curry program, we have to rename
-- their modules to be able to import both into the same program. Renaming the
-- modules also means renaming all references to the modules. And since the
-- package's dependencies can also change between different versions, we have to
-- rename all modules in all transitive dependencies as well. When renaming the
-- modules, we simply prefix them with the version of the original package (i.e.
-- the transitive dependencies get the same prefix as the original package). If
-- we have package versions 1.0.0 and 1.1.0 and our module is called
-- `Test.Functions`, then we will rename the from version 1.0.0 to
-- `V_1_0_0_Test.Functions` and the one from version 1.1.0 to
-- `V_1_1_0_Test.Functions`.
--
-- We can now import both module versions and call functions from both versions
-- in the same Curry program. We still have a problem with property tests that
-- are parameterized over a data type present in one of the packages or one of
-- its dependencies:
--
-- ```
-- test_sayHello :: SayHello.MyType -> Test.Prop.Prop
-- test_sayHello x0 = V_1_0_0_SayHello.sayHello x0 <~> V_1_1_0_SayHello.sayHello x0
-- ```
--
-- In this scenario, the parameter type cannot remain `SayHello.MyType`, since
-- we renamed both versions of the module and they each have their own version
-- of the type, `V_1_0_0_SayHello.MyType` and `V_1_1_0_SayHello.MyType`. If we
-- choose one of the renamed types, we cannot give it to the function from the
-- other version of the module as-is. So we generate translator functions that
-- can translate one version of the data type into the other, using
-- `genTranslatorFunction`.
--
-- The comments in this module refer to version A and version B of the module
-- and/or package. Which version is which (e.g. whether A is the smaller
-- version) is irrelevant.

--- Contains information from the package preparation (moving to temp directory
--- and renaming).
data ComparisonInfo = ComparisonInfo
  { infPackageA :: Package  --- A version of package
  , infPackageB :: Package  --- B version of package
  , infDirA :: String       --- Directory where renamed A version is stored
  , infDirB :: String       --- Directory where renamed B version is stored
  , infSourceDirA :: String --- Directory where original A version is stored
  , infSourceDirB :: String --- Directory where original B version is stored
  , infPrefixA :: String    --- Prefix for modules in A version
  , infPrefixB :: String    --- Prefix for modules in B version
  , infModMapA :: [(String, String)] --- Map from old to new module names, ver A
  , infModMapB :: [(String, String)] --- Map from old to new module names, ver B
  }

--- Create temporary directory for the behavior diff.
createBaseTemp :: IO String
createBaseTemp = do
  tmpDir <- getTemporaryDirectory
  let tmp = tmpDir </> "CPM" </> "bdiff"
  recreateDirectory tmp
  return tmp

--- Get temporary directory for the behavior diff.
getBaseTemp :: IO String
getBaseTemp = do
  tmpDir <- getTemporaryDirectory
  return $ tmpDir </> "CPM" </> "bdiff"

--- This message is printed before CurryCheck is executed.
infoText :: String
infoText = unlines
  [ "Running behavior diff where the raw output of CurryCheck is shown."
  , "The test operations are named after the operations they compare."
  , "If a test fails, their implementations semantically differ." ]

--- Compare the behavior of two package versions using CurryCheck.
---
--- @param cfg - the CPM configuration
--- @param repo - the central package index
--- @param gc - the global package cache
--- @param info - the comparison info obtained from preparePackageDirs
--- @param groundequiv - test ground equivalence only?
--- @param useanalysis - use program analysis to filter non-term. operations?
--- @param mods - a list of modules to compare
diffBehavior :: Config
             -> Repository
             -> GC.GlobalCache
             -> ComparisonInfo
             -> Bool
             -> Bool
             -> Maybe [String]
             -> ErrorLogger ()
diffBehavior cfg repo gc info groundequiv useanalysis cmods = do
  baseTmp <- liftIOEL getBaseTemp
  (acyCache, loadpath, funcs, removed) <-
    findFunctionsToCompare cfg repo gc (infSourceDirA info) (infSourceDirB info)
                          useanalysis cmods
  let filteredFuncs =
        maybe funcs
              (\mods -> filter ((`elem` mods) . fst . funcName . snd) funcs)
              cmods
      filteredNames = map snd filteredFuncs
  logDebug ("Filtered operations to be checked: " ++
                  showFuncNames filteredNames)
  case funcs of
    [] -> liftIOEL (printRemoved removed >> return ())
    _  -> do
      liftIOEL $ do
         putStrLn infoText
         printRemoved removed
         putStrLn $
           "Comparing operations " ++ showFuncNames filteredNames ++ "\n"
      genCurryCheckProgram cfg repo gc filteredFuncs info groundequiv
                            acyCache loadpath
      callCurryCheck cfg info baseTmp
 where
   printRemoved removed =
     if null removed then return ()
                     else putStrLn (renderRemoved removed) >> putStrLn ""

--- Renders the list of functions that were excluded from the comparison along
--- with reasons for their exclusion.
renderRemoved :: [(CFuncDecl, FilterReason)] -> String
renderRemoved rs =
  pPrint $ text "The following operations are not compared:" <$$>
  vcat (map renderReason rs)
 where
  renderReason (f, r) = indent 4 $ (text $ showQName (funcName f)) <+>
                                   text "-" <+> reasonText r
  reasonText NoReason = text "Unknown reason"
  reasonText Diffing = text "Different function types or function missing"
  reasonText NonMatchingTypes = text "Some types inside the function type differ"
  reasonText HighArity = text "Arity too high"
  reasonText IOAction  = text "IO action"
  reasonText NoCompare = text "Marked NOCOMPARE"
  reasonText FuncArg   = text "Takes functions as arguments"
  reasonText NonTerm   = text "Possibly non-terminating"

--- Runs CurryCheck on the generated program.
callCurryCheck :: Config -> ComparisonInfo -> String -> ErrorLogger ()
callCurryCheck _ info baseTmp = do
  oldPath <- liftIOEL $ getEnv "CURRYPATH"
  let currypath = infDirA info ++ ":" ++ infDirB info
  mbccfile <- liftIOEL $ getFileInPath "curry-check"
  ecode <-
    maybe
      (do logInfo "CurryCheck not found, no comparison performed"
          return 0)
      (\cc -> do
        liftIOEL $ setEnv "CURRYPATH" currypath
        logDebug $ "Run `curry-check Compare' in `" ++ baseTmp ++ "' with"
        logDebug $ "CURRYPATH=" ++ currypath
        ec <- inDirectoryEL baseTmp $ showExecCmd (cc ++ " Compare")
        liftIOEL $ setEnv "CURRYPATH" oldPath
        logDebug "CurryCheck finished"
        return ec)
      mbccfile
  if ecode==0
    then return ()
    else logError "CurryCheck detected behavior error!"

--- Generates a program containing CurryCheck tests that will compare the
--- behavior of the given functions. The program will be written to the
--- `Compare.curry` file in the behavior diff temp directory.
genCurryCheckProgram :: Config
                     -> Repository
                     -> GC.GlobalCache
                     -> [(Bool,CFuncDecl)]
                     -> ComparisonInfo
                     -> Bool
                     -> ACYCache -> [String]
                     -> ErrorLogger ()
genCurryCheckProgram cfg repo gc prodfuncs info groundequiv acyCache loadpath = do
  baseTmp <- liftIOEL $ getBaseTemp
  let translatorGenerator = uncurry $ genTranslatorFunction cfg repo gc info
  (_, transMap) <- foldM translatorGenerator (acyCache, emptyTrans)
                     translateTypes
  let (limittypes,testFunctions) =
         unzip (map (genTestFunction info groundequiv transMap) prodfuncs)
  let transFunctions = transFuncs transMap
  let limittconss    = nub (concatMap tconsOfType (concat limittypes))
  let limittcmods    = nub (map fst limittconss)
  -- get the declarations of all types which require limit functions:
  (_, limittdecls) <- foldM addLimitType (acyCache,[]) limittconss
  typeinfos <- analyzeModules "recursive type" typesInValuesAnalysis loadpath
                              limittcmods
  let limitFunctions = concatMap (genLimitFunction typeinfos) limittdecls
      prog = simpleCurryProg "Compare" imports []
               (concat testFunctions ++ transFunctions ++
                (if groundequiv then limitFunctions else []))
               []
  let prodops = map snd (filter fst prodfuncs)
  liftIOEL $ unless (null prodops) $ putStrLn $
    "Productive operations (currently not fully supported for all types):\n" ++
    showFuncNames prodops ++ "\n"
  liftIOEL $ writeFile (baseTmp </> "Compare.curry")
            (progcmts ++ "\n" ++ showCProg prog ++ "\n")
  return ()
 where
  addLimitType (acy,tdecls) qn =
    findTypeInModules cfg repo gc info acy qn >>= \ (acy',tdecl) ->
    return (acy', tdecl:tdecls)

  progcmts = unlines $ map ("-- "++)
    [ "This file contains properties to compare packages"
    , packageId (infPackageA info) ++
      " and " ++ packageId (infPackageB info) ++ "."
    , ""
    , "It should be processed by 'curry-check Compare' with setting"
    , "export CURRYPATH=" ++ infDirA info ++ ":" ++ infDirB info
    ]

  allReferencedTypes =
    nub ((concat $ map (argTypes . typeOfQualType . funcType . snd) prodfuncs)
         ++ map (resultType . typeOfQualType . funcType . snd) prodfuncs)
  translateTypes = filter (needToTranslatePart info) allReferencedTypes

  mods = map (fst . funcName . snd) prodfuncs
  modsA = map (\mod -> (infPrefixA info) ++ "_" ++ mod) mods
  modsB = map (\mod -> (infPrefixB info) ++ "_" ++ mod) mods
  imports = modsA ++ modsB ++ ["Test.Prop"]

--- Generates functions to limit the result depth of values of
--- the given data type.
genLimitFunction :: ProgInfo [QName] -> CTypeDecl -> [CFuncDecl]
genLimitFunction typeinfos tdecl = case tdecl of
  CType tc _ tvs consdecls _ ->
    [stCmtFunc ("Limit operation for type " ++ tcname)
      (transCTCon2Limit tc) (length tvs + 2) Private
      (foldr (~>) (limitFunType (applyTC tc (map CTVar tvs)))
             (map (limitFunType . CTVar) tvs))
      (cdecls2rules tc tvs consdecls)]
  _ -> error $ "Cannot generate limit function for type " ++ tcname
 where
  tcname = showQName (typeName tdecl)

  limitFunType texp = baseType ("Nat","Nat") ~> texp ~> texp

  var2limitfun (i,ti) = (i,"lf"++ti)

  cdecls2rules tc tvs cdecls =
    if null cdecls
      then [simpleRule [CPVar (0,"_"), CPVar (1,"x")] (CVar (1,"x"))]
      else concatMap (cdecl2rules tvs (nullaryConsOf cdecls)) cdecls
   where
    nullaryConsOf [] = error $ "Cannot generate limit operation for types " ++
                               "without nullary constructors: " ++ showQName tc
    nullaryConsOf (CCons qc _ []   : _ ) = qc
    nullaryConsOf (CCons _ _ (_:_) : cs) = nullaryConsOf cs
    nullaryConsOf (CRecord _ _ _   : cs) = nullaryConsOf cs

  cdecl2rules tvs tnull (CCons qc _ texps) =
    let lfunargs = map (CPVar . var2limitfun) tvs
        argvars  = map (\i -> (i,"x"++show i)) [0 .. length texps - 1]
        isRecursive t = t `elem` fromMaybe [] (lookupProgInfo t typeinfos)
        isRecursiveCons = any isRecursive (concatMap tconsOfType texps)
    in
    (if isRecursiveCons
     then [simpleRule (lfunargs ++ [CPComb ("Nat","Z") [],
                                    CPComb qc (map CPVar argvars)])
                      (applyF tnull [])]
     else []) ++
    [simpleRule
      (lfunargs ++ [if isRecursiveCons then CPComb ("Nat","S") [CPVar (0,"n")]
                                       else CPVar (0,"n"),
                    CPComb qc (map CPVar argvars)])
      (applyF qc (map (\ (te,v) -> applyE (type2LimOp te)
                                 [CVar (0,"n"), CVar v]) (zip texps argvars)))]
  cdecl2rules _ _ (CRecord qc _ _) =
    error $ "Cannot generate limit operation for record field " ++ showQName qc

  type2LimOp texp = case texp of
    CTVar tv -> CVar (var2limitfun tv)
    CFuncType _ _ ->
      error "type2LimOp: cannot generate limit operation for function type"
    _ -> maybe (error "type2LimOp: cannot generate limit operation for type application")
               (\ (tc,ts) -> applyF (transCTCon2Limit tc) (map type2LimOp ts))
               (tconsArgsOfType texp)


--- Generates a test function to compare two versions of the given function.
--- If the argument and result types must be transformed between types
--- of the two different version, also auxiliary operations are generated
--- for the equivalence test.
--- If the function is productive, we also return the result type of
--- the function in order to generate "limit" functions for this type.
genTestFunction :: ComparisonInfo -> Bool -> TransMap -> (Bool, CFuncDecl)
                -> ([CTypeExpr], [CFuncDecl])
genTestFunction info groundequiv tm (isprod,f) =
 (if isprod && groundequiv then [newResultTypeA] else [],
  if groundequiv
    then
      [stCmtFunc ("Check ground equivalence of operation " ++ fmod ++ "." ++
                  fname ++ if isprod then " up to a depth limit" else "")
        (modName, testName ++ "_GroundEquiv") (realArity f) Private newType
        [if isprod
           then let limitvar = (100,"limit") in
                simpleRule (if isprod then CPVar limitvar : vars else vars)
                  (applyF (easyCheckMod "<~>")
                    [applyE (type2LimitFunc newResultTypeA)
                            [CVar limitvar, callA],
                     applyE (type2LimitFunc newResultTypeA)
                            [CVar limitvar, callB]])
           else simpleRule vars (applyF (easyCheckMod "<~>") [callA, callB])]
      ]
    else
      [stFunc testName1 (realArity f) Private
              (replaceResultType newType newResultTypeB)
              [simpleRule vars callA]
      ,stFunc testName2 (realArity f) Private
              (replaceResultType newType newResultTypeB)
              [simpleRule vars callB]
      ,stCmtFunc ("Check equivalence of operation " ++ fmod ++ "." ++ fname)
        (modName, testName ++ "_Equivalent") 0 Private
        (baseType (easyCheckMod "Prop"))
        [simpleRule [] (applyF (easyCheckMod "<=>")
                                  [constF testName1, constF testName2])]]
 )
 where
  (fmod,fname) = funcName f
  modName = "Compare"
  both fun (a, b) = (fun a, fun b)
  testName = "test_" ++
       combineTuple (both (replace' '.' '_') $ (fmod, encodeCurryId fname)) "_"
  testName1 = (modName, testName++"_1")
  testName2 = (modName, testName++"_2")
  vars = pVars (realArity f)
  modA = infPrefixA info ++ "_" ++ fmod
  modB = infPrefixB info ++ "_" ++ fmod
  instantiatedFunc = instantiateBool $ typeOfQualType $ funcType f

  newResultTypeA = mapTypes (infModMapA info)
                    (instantiateBool (resultType (typeOfQualType (funcType f))))

  newResultTypeB = mapTypes (infModMapB info)
                    (instantiateBool (resultType (typeOfQualType (funcType f))))

  newType = let ftype = mapTypes (infModMapA info) $ genTestFuncType f
            in if isprod then baseType ("Nat","Nat") ~> ftype
                         else ftype

  returnTransform = case findTrans tm (resultType $ instantiatedFunc) of
    Nothing -> id
    Just tr -> \t -> applyF (modName, tr) [t]

  -- Since we use the data types from the A version in type of the generated
  -- test function, we transform the parameters in the call of the B version of
  -- the tested function using the translator functions from the TransMap. As we
  -- already have translator functions from data type version A to B, we will
  -- translate the result of the A function using these functions. The
  -- comparison of function results will thus be done on the B version of the
  -- types, while the parameter generation will be done on the A version.
  callA = returnTransform $ applyF (modA, fname)
                                   $ map (\(CPVar v) -> CVar v) vars
  callB = applyF (modB, fname) $ map transformedVar
                               $ zip (argTypes $ instantiatedFunc) vars

  transformedVar (texp,exp) = case (texp,exp) of
    (CTVar _, CPVar v) -> CVar v
    (CFuncType _ _, CPVar v) -> CVar v
    (_, CPVar v) ->
      maybe (CVar v)
            (\_ -> case findTrans tm texp of
                     Just  n -> applyF (modName, n) [CVar v]
                     Nothing -> CVar v)
            (tconsArgsOfType texp)
    _ -> error "CPM.Diff.Behavior.transformedVar: This case should be impossible to reach"

-- encode a Curry identifier into an alphanum form:
encodeCurryId :: String -> String
encodeCurryId [] = []
encodeCurryId (c:cs)
  | isAlphaNum c || c == '_' = c : encodeCurryId cs
  | otherwise =  let oc = ord c
    in int2hex (oc `div` 16) : int2hex (oc `mod` 16) : encodeCurryId cs
 where
   int2hex i = if i<10 then chr (ord '0' + i)
                       else chr (ord 'A' + i - 10)

--- Checks if any part of the given type needs to be translated using a
--- translator function.
needToTranslatePart :: ComparisonInfo -> CTypeExpr -> Bool
needToTranslatePart _    (CTVar _) = False
needToTranslatePart info (CFuncType e1 e2) =
  needToTranslatePart info e1 || needToTranslatePart info e2
needToTranslatePart info (CTApply e1 e2) =
  needToTranslatePart info e1 || needToTranslatePart info e2
needToTranslatePart info (CTCons n) =
  isMappedType info n

--- Checks if the module of the given type is one of the mapped modules, i.e.
--- one that is present in two versions.
isMappedType :: ComparisonInfo -> (String, String) -> Bool
isMappedType info (mod, _) = isJust $ lookup mod (infModMapA info)

--- The TransMap contains a map of type expressions to translator function
--- names, as well as the next translator function number and a list of the
--- translator functions themselves.
data TransMap = TransMap [(CTypeExpr, String)] Int [CFuncDecl]

--- An empty TransMap.
emptyTrans :: TransMap
emptyTrans = TransMap [] 0 []

--- Adds an entry to the TransMap. Note that this does not add the
--- function itself. Use `addFunc` to add the function.
addEntry :: TransMap -> CTypeExpr -> (TransMap, String)
addEntry (TransMap m n fs) e =
  (TransMap ((e, "tt_" ++ show n) : m) (n + 1) fs, "tt_" ++ show n)

--- Adds a translator function to the list of functions in the TransMap.
addFunc :: TransMap -> CFuncDecl -> TransMap
addFunc (TransMap m n fs) f = TransMap m n (f:fs)

--- Finds the name of the translator function for a type expression, if it
--- exists.
findTrans :: TransMap -> CTypeExpr -> Maybe String
findTrans (TransMap m _ _) e = lookup e m

--- Gets all translator functions from a TransMap.
transFuncs :: TransMap -> [CFuncDecl]
transFuncs (TransMap _ _ fs) = fs

--- Get type declarations for some types that are namespaced to the Prelude
--- module, but whose type declarations are not actually contained in the
--- Prelude module.
predefinedType :: (String, String) -> Maybe CTypeDecl
predefinedType x = case x of
  ("Prelude", "[]") -> Just $ CType ("Prelude", "[]") Public [(0, "a")] [
      simpleCCons ("Prelude", "[]") Public []
    , simpleCCons ("Prelude", ":") Public
                  [CTVar (0, "a"), listType (CTVar (0, "a"))]] []
  ("Prelude", "(,)") -> Just $ CType ("Prelude", "(,)") Public [(0, "a"), (1, "b")] [
    simpleCCons ("Prelude", "(,)") Public [CTVar (0, "a"), CTVar (1, "b")]] []
  ("Prelude", "(,,)") -> Just $ CType ("Prelude", "(,,)") Public [(0, "a"), (1, "b"), (2, "c")] [
    simpleCCons ("Prelude", "(,,)") Public [CTVar (0, "a"), CTVar (1, "b"), CTVar (2, "c")]] []
  ("Prelude", "(,,,)") -> Just $ CType ("Prelude", "(,,,)") Public [(0, "a"), (1, "b"), (2, "c"), (3, "d")] [
    simpleCCons ("Prelude", "(,,,)") Public [CTVar (0, "a"), CTVar (1, "b"), CTVar (2, "c"), CTVar (3, "d")]] []
  _ -> Nothing

--- The ACYCache caches the AbstractCurry representations of Curry modules,
--- specific to the directory it is stored in (to support multiple versions of a
--- module).
data ACYCache = ACYCache [(String, [(String, CurryProg)])]

--- An empty ACYCache.
emptyACYCache :: ACYCache
emptyACYCache = ACYCache []

--- Finds a module inside an ACYCache, regardless of its directory.
findModule :: String -> ACYCache -> Maybe CurryProg
findModule mod (ACYCache ps) = case lookup mod ps of
  Nothing -> Nothing
  Just ms -> listToMaybe $ map snd ms

--- Finds a module inside the ACYCache that was read from a specific directory.
findModuleDir :: String -> String -> ACYCache -> Maybe CurryProg
findModuleDir dir mod (ACYCache ps) = case lookup mod ps of
  Nothing -> Nothing
  Just ms -> lookup dir ms

--- Adds a module to the ACYCache without a directory.
addModule :: String -> CurryProg -> ACYCache -> ACYCache
addModule mod p (ACYCache ps) = case lookup mod ps of
  Just  _ -> ACYCache ps
  Nothing -> ACYCache $ (mod, [("", p)]):ps

--- Adds a module to the ACYCache with a directory.
addModuleDir :: String -> String -> CurryProg -> ACYCache -> ACYCache
addModuleDir dir mod p (ACYCache ps) = case lookup mod ps of
  Just ms -> case lookup dir ms of
    Just  _ -> ACYCache ps
    Nothing -> ACYCache $ (mod, (dir, p):ms):(delete (mod, ms) ps)
  Nothing -> ACYCache $ (mod, [(dir, p)]):ps

--- Generate a translator function for a type expression. Expects a CTCons.
---
--- @param cfg current CPM configuration
--- @param repo package repository
--- @param gc the global package cache
--- @param info information about the current comparison
--- @param tm the map of translator functions
--- @param e the type expression to generate a translator for
genTranslatorFunction :: Config
                      -> Repository
                      -> GC.GlobalCache
                      -> ComparisonInfo
                      -> ACYCache
                      -> TransMap
                      -> CTypeExpr
                      -> ErrorLogger (ACYCache, TransMap)
genTranslatorFunction cfg repo gc info acy tm texp =
-- TODO: generate also translation functions for functional types.
-- This requires type translator in both directions but currently
-- we generate only one direction.
-- For instance, to translate a function A->B into A'->B':
-- (A->B)2(A'->B') f = \x -> B2B' (f (A'2A x))
 let (mod, n) = maybe
        (error $ "CPM.Diff.Behavior.genTranslatorFunction: " ++
                 "cannot generate type translation function for type:\n" ++
                 pPrint (ppCTypeExpr defaultOptions texp))
        fst
        (tconsArgsOfType texp)
 in
  -- Don't generate another translator if there already is one for the current
  -- type.
  if isJust $ findTrans tm t'
    then return (acy, tm)
    else findTypeInModules cfg repo gc info acy (mod,n) >>=
  -- We want to work on the constructors with all type variables instantiated
  -- with the types from the type that we're supposed to build a translator for.
  \(acy', typeDecl) -> (return $ instantiate typeDecl t') >>=
  -- Add the entry at this point to make sure that it's available when we
  -- generate the other translators and if we need to call it recursively later
  -- on.
  \instTypeDecl -> (return $ addEntry tm t') >>=
  \(tm', name) -> foldM (uncurry $ genTranslatorFunction cfg repo gc info)
                         (acy', tm') (transExprs instTypeDecl) >>=
  \(acy'', tm'') ->
    let
      aType = prefixMappedTypes (infPrefixA info) t'
      bType = prefixMappedTypes (infPrefixB info) t'
      fType = CFuncType aType bType
      fName = ("Compare", name)
      mapIfNeeded modMap m =
        if isMappedType info (m, "") then fromJust $ lookup m modMap
                                     else m
      mapIfNeededA = mapIfNeeded (infModMapA info)
      mapIfNeededB = mapIfNeeded (infModMapB info)

      transformer (i,te) = case te of
        CTVar _ -> CVar (i, "x" ++ show i)
        CFuncType _ _ -> CVar (i, "x" ++ show i)
        _ -> case findTrans tm'' te of
               Nothing -> CVar (i, "x" ++ show i)
               Just tn -> applyF ("Compare", tn) [CVar (i, "x" ++ show i)]

      ruleForCons (CCons (m, cn) _ es) = simpleRule [pattern] call
       where
        pattern = CPComb (mapIfNeededA m, cn) (pVars (length es))
        -- Apply constructor from B, calling translator functions if neccessary.
        call = applyF (mapIfNeededB m, cn) $ map transformer
                                           $ zip (take (length es) [0..]) es
      ruleForCons (CRecord (m, cn) _ fs) = simpleRule [pattern] call
       where
        pattern = CPComb (mapIfNeededA m, cn) (pVars (length fs))
        call = applyF (mapIfNeededB m, cn) $ map transformer
               $ zip (take (length fs) [0..]) (map (\(CField _ _ es) -> es) fs)

      synRule e = simpleRule [CPVar (0, "x0")] call
       where
        call = transformer (0, e)
    in case instTypeDecl of
      CType _ _ _ cs _ -> return $
        (acy'', addFunc tm'' (stFunc fName 1 Public fType (map ruleForCons cs)))
      CTypeSyn _ _ _ e -> return $
        (acy'', addFunc tm'' (stFunc fName 1 Public fType [synRule e]))
      CNewType _ _ _ c _ -> return $
        (acy'', addFunc tm'' (stFunc fName 1 Public fType [ruleForCons c]))
 where
  -- Since our test functions always use polymorphic types instantiated to Bool,
  -- we generate our translator functions for Bool-instantiated types as well.
  t' = instantiateBool texp

  -- Finds all type expressions in the instantiated constructors that contain
  -- types that need to be translated.
  transExprs cs = filter (needToTranslatePart info) $ nub $ extractExprs cs
  extractExprs (CType _ _ _ es _) = concat $ map extractExprsCons es
  extractExprs (CTypeSyn _ _ _ e) = [e]
  extractExprs (CNewType _ _ _ c _) = extractExprsCons c
  extractExprsCons (CCons _ _ es) = es
  extractExprsCons (CRecord _ _ fs) = map (\(CField _ _ es) -> es) fs

  -- Recursively prefixes those types which are present in two versions.
  prefixMappedTypes pre (CTCons (mod', n')) =
    if isMappedType info (mod', n')
      then CTCons (pre ++ "_" ++ mod', n')
      else CTCons (mod', n')
  prefixMappedTypes _   (CTVar v) = CTVar v
  prefixMappedTypes pre (CFuncType e1 e2) =
    CFuncType (prefixMappedTypes pre e1) (prefixMappedTypes pre e2)
  prefixMappedTypes pre (CTApply e1 e2) =
    CTApply (prefixMappedTypes pre e1) (prefixMappedTypes pre e2)

-- Finds the type declaration for a given qualified type constructor.
-- If the module is not in the ACYCache, it is read and added to the cache.
findTypeInModules :: Config -> Repository -> GC.GlobalCache -> ComparisonInfo
                  -> ACYCache -> QName -> ErrorLogger (ACYCache, CTypeDecl)
findTypeInModules cfg repo gc info acy (mod,n) =
  case predefinedType (mod, n) of
    Just ty -> return (acy, ty)
    Nothing ->
     (case findModule mod acy of
       Just  p -> return $ p
       Nothing -> resolveAndCopyDependencies cfg repo gc
                                          (infSourceDirA info) >>= \deps ->
                  readAbstractCurryFromDeps (infSourceDirA info) deps mod >>=
                                                       return) >>= \prog ->
                  case filter ((== n) . snd . typeName) (types prog) of
                    [] -> fail $ "No type defined '" ++ n ++ "' in module '"
                                   ++ mod ++ "'"
                    (x:_) -> return (addModule mod prog acy, x)

--- Replaces type variables with their expression in the map if there is one,
--- leaves them alone otherwise.
maybeReplaceVar :: [(CVarIName, CTypeExpr)] -> CTypeExpr -> CTypeExpr
maybeReplaceVar vm (CTVar v) = case lookup v vm of
  Nothing -> CTVar v
  Just e' -> e'
maybeReplaceVar _  (CTCons n) = CTCons n
maybeReplaceVar vm (CFuncType e1 e2) =
  CFuncType (maybeReplaceVar vm e1) (maybeReplaceVar vm e2)
maybeReplaceVar vm (CTApply e1 e2) =
  CTApply (maybeReplaceVar vm e1) (maybeReplaceVar vm e2)

--- Instantiates all constructors of a type declaration with the types from a
--- constructor type expression. Type variables that are not used in the
--- constructor referenced by the type expression remain as they are.
instantiate :: CTypeDecl -> CTypeExpr -> CTypeDecl
instantiate tdecl texp = case texp of
  CTVar _ -> error "CPM.Diff.Behavior.instantiate: Cannot instantiate CTVar"
  CFuncType _ _ -> error "CPM.Diff.Behavior.instantiate: Cannot instantiate CFuncType"
  _ -> maybe (error "CPM.Diff.Behavior.instantiate: Cannot instantiate CTApply")
             (\ (_,texps) -> instantiate' tdecl texps)
             (tconsArgsOfType texp)
 where
  instantiate' (CType n v vs cs d) es = CType n v vs (map cons cs) d
   where
    varMap = zip vs es
    cons (CCons n' v' es') =
          CCons n' v' $ map (maybeReplaceVar varMap) es'
    cons (CRecord n' v' fs') =
          CRecord n' v' $ map maybeReplaceField fs'
    maybeReplaceField (CField n'' v'' e) =
      CField n'' v'' $ maybeReplaceVar varMap e
  instantiate' (CTypeSyn n v vs e) es =
      CTypeSyn n v vs $ maybeReplaceVar varMap e
   where
    varMap = zip vs es
  instantiate' (CNewType n v vs c d) es = CNewType n v vs (cons c) d
   where
    varMap = zip vs es
    cons (CCons n' v' es') =
          CCons n' v' $ map (maybeReplaceVar varMap) es'
    cons (CRecord n' v' fs') =
          CRecord n' v' $ map maybeReplaceField fs'
    maybeReplaceField (CField n'' v'' e) =
      CField n'' v'' $ maybeReplaceVar varMap e


--- Recursively transforms the module names of all type constructors in a
--- type expression into new module names according to a mapping of
--- module names.
mapTypes :: [(String,String)] -> CTypeExpr -> CTypeExpr
mapTypes mmap (CFuncType a b) = CFuncType (mapTypes mmap a) (mapTypes mmap b)
mapTypes mmap (CTApply a b)   = CTApply (mapTypes mmap a) (mapTypes mmap b)
mapTypes _    v@(CTVar _)     = v
mapTypes mmap (CTCons (m, n)) =
  case lookup m mmap of
    Nothing -> CTCons (m, n)
    Just m' -> CTCons (m', n)

realArity :: CFuncDecl -> Int
realArity (CFunc _ _ _ t _) = arityOfType (typeOfQualType t)
realArity (CmtFunc _ _ _ _ t _) = arityOfType (typeOfQualType t)

arityOfType :: CTypeExpr -> Int
arityOfType (CFuncType _ b) = 1 + arityOfType b
arityOfType (CTVar _) = 0
arityOfType (CTCons _) = 0
arityOfType (CTApply _ _) = 0

-- Wrap an expression of a given type with a call to a corresponding
-- depth-limit function:
type2LimitFunc :: CTypeExpr -> CExpr
type2LimitFunc texp = case texp of
  CTVar _ ->
    error "type2LimitFunc: cannot generate limit operation for type variable"
  CFuncType _ _ ->
    error "type2LimitFunc: cannot generate limit operation for function type"
  _ -> maybe
        (error
        "type2LimitFunc: cannot generate limit operation for type application")
        (\ (tc,ts) -> applyF (transCTCon2Limit tc) (map type2LimitFunc ts))
        (tconsArgsOfType texp)

-- Translate a type constructor name to the name of the corresponding limit
-- operation:
transCTCon2Limit :: QName -> QName
transCTCon2Limit (_,tcn) = ("Compare", "limit" ++ trans tcn)
 where
  trans n | n=="[]"             = "List"
          | n=="()"             = "Unit"
          | "(," `isPrefixOf` n = "Tuple" ++ show (length n - 1)
          | otherwise           = n

--- Qualify a name by `Test.Prop` module:
easyCheckMod :: String -> QName
easyCheckMod n = ("Test.Prop", n)

--- Generates a function type for the test function by replacing the result
--- type with `Test.Prop.Prop`. Also instantiates polymorphic types to
--- Bool.
genTestFuncType :: CFuncDecl -> CTypeExpr
genTestFuncType f = replaceResultType t (baseType (easyCheckMod "Prop"))
  where t = instantiateBool $ typeOfQualType $ funcType f

--- Instantiates all type variables in a type expression to `Prelude.Bool`.
instantiateBool :: CTypeExpr -> CTypeExpr
instantiateBool (CTVar _) = boolType
instantiateBool (CTCons n) = CTCons n
instantiateBool (CTApply a b) = CTApply (instantiateBool a) (instantiateBool b)
instantiateBool (CFuncType a b) =
  CFuncType (instantiateBool a) (instantiateBool b)

--- Replaces the result type of a function type.
replaceResultType :: CTypeExpr -> CTypeExpr -> CTypeExpr
replaceResultType (CFuncType a (CTVar _)) z = CFuncType a z
replaceResultType (CFuncType a (CTCons _)) z = CFuncType a z
replaceResultType (CFuncType a (CTApply _ _)) z = CFuncType a z
replaceResultType (CFuncType a b@(CFuncType _ _)) z =
  CFuncType a (replaceResultType b z)
replaceResultType (CTVar _) z = z
replaceResultType (CTCons _) z = z
replaceResultType (CTApply _ _) z = z

combineTuple :: (String, String) -> String -> String
combineTuple (a, b) s = a ++ s ++ b

showQName :: QName -> String
showQName qn = combineTuple qn "."

showFuncNames :: [CFuncDecl] -> String
showFuncNames = intercalate ", " . map (showQName . funcName)

replace' :: Eq a => a -> a -> [a] -> [a]
replace' _ _ [] = []
replace' o n (x:xs) | x == o = n : replace' o n xs
                    | otherwise = x : replace' o n xs

------------------------------------------------------------------------------
--- Finds a list of functions that can be compared. At the moment, this uses the
--- functionality from `CPM.Diff.API` to compare the public interfaces of both
--- module versions and find the functions that have not changed between
--- versions.
---
--- @param cfg the CPM configuration
--- @param repo the current repository
--- @param gc the global package cache
--- @param dirA the directory of the A version of the package
--- @param dirB the directory of the B version of the package
--- @param useanalysis - use program analysis to filter non-term. operations?
--- @param mods - the modules to compare (if Nothing, compare exported modules)
--- @return a tuple consisting of an ACYCache, a list of functions to
---         be compared (with a flag which is true if they are productive,
---         might be non-terminating but can be compared level-wise),
---         and a list of non-comparable functions with a reason
findFunctionsToCompare :: Config
                       -> Repository
                       -> GC.GlobalCache
                       -> String
                       -> String
                       -> Bool
                       -> Maybe [String]
                       -> ErrorLogger (ACYCache, [String],
                              [(Bool,CFuncDecl)], [(CFuncDecl, FilterReason)])
findFunctionsToCompare cfg repo gc dirA dirB useanalysis onlymods = do
  pkgA <- loadPackageSpec dirA
  pkgB <- loadPackageSpec dirB
  depsA <- resolveAndCopyDependencies cfg repo gc dirA
  let cmods = intersect (exportedModules pkgA) (exportedModules pkgB)
  let mods = maybe cmods (intersect cmods) onlymods
  if null mods
   then logInfo "No exported modules to compare" >>
        return (emptyACYCache,[],[],[])
   else do
    logInfo ("Comparing modules: "++ intercalate " " mods)
    diffs <- APIDiff.compareModulesInDirs cfg repo gc dirA dirB (Just mods)
    (acy, allFuncs) <- findAllFunctions dirA dirB pkgA depsA emptyACYCache mods
    logDebug ("All public functions: " ++ showFuncNames allFuncs)
    let areDiffThenFilter        = thenFilter allFuncs Diffing
    let areHighArityThenFilter   = thenFilter allFuncs HighArity
    let areIOActionThenFilter    = thenFilter allFuncs IOAction
    let areNoCompareThenFilter   = thenFilter allFuncs NoCompare
    let areNonMatchingThenFilter = thenFilter allFuncs NonMatchingTypes
    let haveFuncArgThenFilter    = thenFilter allFuncs FuncArg
    (emptyFilter ((liftFilter $ filterDiffingFunctions diffs) acy allFuncs)
                            `areDiffThenFilter`
        liftFilter filterHighArity `areHighArityThenFilter`
        liftFilter filterIOAction  `areIOActionThenFilter`
        filterNoCompare        dirA dirB depsA `areNoCompareThenFilter`
        filterNonMatchingTypes dirA dirB depsA `areNonMatchingThenFilter`
        filterFuncArg          dirA dirB depsA `haveFuncArgThenFilter`
        liftFilter id ) >>= terminationFilter pkgA dirA depsA useanalysis

--- Filters out functions which are possibly non-terminating and
--- non-productive, and mark productive functions so that they are
--- tested not by standard equality.
terminationFilter :: Package -> String -> [Package] -> Bool
                  -> (ACYCache, [CFuncDecl], [(CFuncDecl, FilterReason)])
                  -> ErrorLogger (ACYCache, [String], [(Bool,CFuncDecl)],
                                  [(CFuncDecl, FilterReason)])
terminationFilter _ _ _ False (a,fs,rm) =
  return (a, [], map (\f->(False,f)) fs, rm)
terminationFilter pkgA dirA depsA True (acy, funcs, rm) = do
  let currypath = loadPathForPackage pkgA dirA depsA
      mods = nub (map (fst . funcName) funcs)
  ainfo <- analyzeModules "productivity" productivityAnalysis currypath mods
  -- compute functions which should be definitely compared (due to TERMINATE
  -- or PRODUCTIVE pragmas):
  modscmts <- liftIOEL $ mapM (getCompare currypath) mods
  let termfuns = concatMap (\md -> md ("TERMINATE"  `isInfixOf`)) modscmts
      prodfuns = concatMap (\md -> md ("PRODUCTIVE" `isInfixOf`)) modscmts
  logDebug ("Functions marked with TERMINATE: " ++ showFuncNames termfuns)
    >> return ()
  logDebug ("Functions marked with PRODUCTIVE: " ++ showFuncNames prodfuns)
    >> return ()
  let infoOf f = fromMaybe Looping (lookupProgInfo (funcName f) ainfo)
      ntfuncs  = filter (\f -> infoOf f == Looping  &&
                               f `notElem` termfuns && f `notElem` prodfuns)
                        funcs
  return (acy, currypath,
             map (\f -> (not (infoOf f == Terminating || f `elem` termfuns), f))
                 (funcs \\ ntfuncs),
             rm ++ map (\f -> (f,NonTerm)) ntfuncs)
 where
  --- Get functions in a module satisfying a given predicate on pragma comments
  getCompare currypath modname = do
    src <- lookupModuleSource currypath modname
    (_,comments) <- case src of
      Nothing        -> error $ "Module not found: " ++ modname
      Just (_, file) -> readComments file
    return (\p -> filter (\f -> let (mn,fn) = funcName f
                          in mn == modname &&
                             p (getFuncComment fn comments))
                         funcs)

-- Analyze a list of modules with some static program analysis in a given
-- load path. Returns the combined analysis information.
-- Raises an error if something goes wrong.
analyzeModules :: (Read a, Show a, ReadWrite a, Eq a)
               => String -> Analysis a -> [String] -> [String]
               -> ErrorLogger (ProgInfo a)
analyzeModules ananame analysis currypath mods = do
  logDebug ("Running " ++ ananame ++ " analysis on modules: " ++
             intercalate ", " mods)
  logDebug ("CURRYPATH=" ++ joinSearchPath currypath)
  anainfos <- liftIOEL $ mapM (analyzeModule analysis currypath) mods
  logDebug "Analysis finished"
  return $ foldr combineProgInfo emptyProgInfo anainfos

-- Analyze a module with some static program analysis in a given
-- load path. Raises an error if something goes wrong.
analyzeModule :: (Read a, Show a, ReadWrite a, Eq a)
              => Analysis a -> [String] -> String -> IO (ProgInfo a)
analyzeModule analysis currypath mod = do
  setEnv "CURRYPATH" (joinSearchPath currypath)
  aresult <- analyzeGeneric analysis mod
  unsetEnv "CURRYPATH"
  either return
         (\e -> do putStrLn "WARNING: error occurred during analysis:"
                   putStrLn e
                   putStrLn "Ignoring analysis information"
                   return emptyProgInfo)
         aresult


emptyFilter :: ErrorLogger (ACYCache, [CFuncDecl])
      -> ErrorLogger (ACYCache, [CFuncDecl], [(CFuncDecl, FilterReason)])
emptyFilter st = st >>= \(a, fs) -> return (a, fs, [])

--- Reasons why a function can be excluded from the list of functions to be
--- compared.
data FilterReason = NoReason
                  | HighArity
                  | IOAction
                  | NoCompare
                  | NonMatchingTypes
                  | Diffing
                  | FuncArg
                  | NonTerm

--- Chain filter functions and mark the ones removed by the previous filter
--- with a given reason.
thenFilter :: [CFuncDecl]
       -> FilterReason
       -> ErrorLogger (ACYCache, [CFuncDecl], [(CFuncDecl, FilterReason)])
       -> (ACYCache -> [CFuncDecl] -> ErrorLogger (ACYCache, [CFuncDecl]))
       -> ErrorLogger (ACYCache, [CFuncDecl], [(CFuncDecl, FilterReason)])
thenFilter allFuncs r st f =
  st >>=
  \(a, fs, rm) -> f a fs >>=
  \(a', fs') -> return (a', fs', rm ++ zip (findMissing rm fs) (repeat r))
 where
  findMissing rm fs = (allFuncs \\ (map fst rm)) \\ fs

--- Lifts a simple filter to a filter that executes inside the IO monad and
--- takes an ACYCache.
liftFilter :: ([CFuncDecl] -> [CFuncDecl])
       -> (ACYCache -> [CFuncDecl] -> ErrorLogger (ACYCache, [CFuncDecl]))
liftFilter f = \a fs -> return (a, f fs)

--- Excludes those functions which take a functional argument, either directly
--- or via a nested type.
filterFuncArg :: String -> String -> [Package] -> ACYCache -> [CFuncDecl]
              -> ErrorLogger (ACYCache, [CFuncDecl])
filterFuncArg = filterFuncsDeep checkFunc
 where
  checkFunc (CFuncType _ _) = True
  checkFunc (CTVar _)       = False
  checkFunc (CTCons _)      = False
  checkFunc (CTApply _ _)   = False

--- Filters functions via a predicate on their argument types. Checks the
--- predicates on nested types as well.
filterFuncsDeep :: (CTypeExpr -> Bool) -> String -> String -> [Package]
                -> ACYCache -> [CFuncDecl]
                -> ErrorLogger (ACYCache, [CFuncDecl])
filterFuncsDeep tpred dirA _ deps acy allFuncs =
  foldM checkFunc (acy, [], []) allFuncs >>=
  \(acy', _, fns) -> return (acy', fns)
 where
  findType n m = case predefinedType n of
    Nothing -> find ((== n) . typeName) $ filter isTypePublic $ types m
    Just ty -> Just ty

  checkFunc (a, c, fs) f =
    (foldM checkTypeExpr (a, c, False) $ argTypes $ typeOfQualType $ funcType f) >>=
    \(a', c', r) -> if r then return (a', c', fs)
                         else return (a', c', f:fs)

  checkTypeExpr (a, c, r) t@(CFuncType e1 e2) =
    if t `elem` c
      then return (a, c, r)
      else if tpred t
             then return (a, c, True)
             else checkTypeExpr (a, c, r) e1 >>=
                  \ (a', c', r') -> checkTypeExpr (a', e1:c', r') e2 >>=
                  \ (a'', c'', r'') -> return (a'', e2:c'', r || r' || r'')
  checkTypeExpr (a, c, r) t@(CTApply e1 e2) =
    if t `elem` c
      then return (a, c, r)
      else if tpred t
             then return (a, c, True)
             else checkTypeExpr (a, c, r) e1 >>=
                  \ (a', c', r') -> checkTypeExpr (a', e1:c', r') e2 >>=
                  \ (a'', c'', r'') -> return (a'', e2:c'', r || r' || r'')
  checkTypeExpr (a, c, r) (CTVar _) = return (a, c, r)
  checkTypeExpr (a, c, r) t@(CTCons n@(mod, _)) =
    if t `elem` c
      then return (a, c, r)
      else if tpred t
             then return (a, c, True)
             else return (a, c, r) >>=
                  \(a', c', _) -> readCached dirA deps a' mod >>=
                  \(a'', prog) -> case findType n prog of
                    Nothing -> fail $ "Type '" ++ show n ++ "' not found."
                    Just t' -> checkType a'' (t:c') t' >>=
                           \(a''', c'', r'') -> return (a''', c'', r || r'')

  checkType a ts (CType _ _ _ cs _)   = foldM checkCons (a, ts, False) cs
  checkType a ts (CTypeSyn _ _ _ e)   = checkTypeExpr (a, ts, False) e
  checkType a ts (CNewType _ _ _ c _) = checkCons (a, ts, False) c

  checkCons (a, ts, r) (CCons _ _ es) = foldM checkTypeExpr (a, ts, r) es
  checkCons (a, ts, r) (CRecord _ _ fs) =
    let es = map (\(CField _ _ e) -> e) fs
    in foldM checkTypeExpr (a, ts, r) es

--- Filters out functions marked with the NOCOMPARE pragma.
filterNoCompare :: String -> String -> [Package] -> ACYCache -> [CFuncDecl]
                -> ErrorLogger (ACYCache, [CFuncDecl])
filterNoCompare dirA dirB _ a fs = liftIOEL $ do
  allCommentsA <- mapM (readComments . modPath dirA) modules
  allCommentsB <- mapM (readComments . modPath dirB) modules
  let commentsA = funcsWithComments $ zip modules allCommentsA
  let commentsB = funcsWithComments $ zip modules allCommentsB
  return (a, filter (not . noCompare commentsA commentsB) fs)
 where
  modules = nub $ map (fst . funcName) fs
  modPath dir mod = dir </> "src" </> joinPath (splitOn "." mod) ++ ".curry"
  -- Zip up all functions with their respective comments.
  funcsWithComments cmts = zip fs (map (getFuncComment' cmts) fs)
  getFuncComment' cmts f =
    let
      mname = fst $ funcName f
      lname = snd $ funcName f
    in case lookup mname cmts of
      Nothing -> ""
      Just cs -> getFuncComment lname $ snd cs
  noCompare cmtsA cmtsB f = noCompare' cmtsA f || noCompare' cmtsB f
  -- Check if NOCOMPARE is mentioned in the comments
  noCompare' cmts f = case lookup f cmts of
    Nothing -> False
    Just  c -> "NOCOMPARE" `isInfixOf` c


--- Removes all functions that have more than five arguments (currently the
--- maximum number of parameters that CurryCheck supports in property tests).
filterHighArity :: [CFuncDecl] -> [CFuncDecl]
filterHighArity = filter ((<= 5) . length . argTypes . typeOfQualType . funcType)

--- Removes all IO actions since they cannot be compared as
--- properties in CurryCheck.
filterIOAction :: [CFuncDecl] -> [CFuncDecl]
filterIOAction = filter (not . isIOType . resultType . typeOfQualType . funcType)

--- Removes all functions that have a diff associated with their name from the
--- given list of functions.
---
--- @param fs the functions to filter
--- @param ds a list of pairs of module names and diffs
filterDiffingFunctions :: [(String, Differences)] -> [CFuncDecl] -> [CFuncDecl]
filterDiffingFunctions diffs allFuncs = nub $ concatMap filterModule modules
 where
  modules = nub $ map (fst . funcName) allFuncs
  diffsForModule mod = case lookup mod diffs of
    Nothing -> []
    Just (_, funcDiffs, _, _) -> map funcDiffName funcDiffs
  funcDiffName (Addition f) = funcName f
  funcDiffName (Removal  f) = funcName f
  funcDiffName (Change _ f) = funcName f
  filterModule mod = filter (not . (`elem` (diffsForModule mod)) . funcName)
                            (funcsForModule mod)
  funcsForModule mod = filter ((== mod) . fst . funcName) allFuncs

--- Excludes those functions whose types do not match in both versions. Checks
--- nested types.
filterNonMatchingTypes :: String -> String -> [Package] -> ACYCache
                      -> [CFuncDecl] -> ErrorLogger (ACYCache, [CFuncDecl])
filterNonMatchingTypes dirA dirB deps acyCache allFuncs =
  foldM funcTypesCompatible (acyCache, [], []) allFuncs >>=
  \(acy, _, fns) -> return (acy, fns)
 where
  allTypes f = let ft = typeOfQualType (funcType f)
               in (resultType ft) : (argTypes ft)
  onlyCons = filter isConsType
  funcTypesCompatible (a, seen, fs) f =
    (foldM typesCompatible (a, seen, True) $ onlyCons $ allTypes f) >>=
    \(a', seen', c) -> if c
      then return (a', seen', f:fs)
      else return (a', seen', fs)
  typesCompatible (a, seen, r) t = case lookup t seen of
    Just b  -> return (a, seen, b && r)
    Nothing -> typesEqual t dirA dirB deps a [] >>=
      \(a', r') -> return (a', ((t, r'):seen), r' && r)

--- Compares the declarations of types mentioned in a type expression
--- recursively. Returns False if the types are different.
typesEqual :: CTypeExpr -> String -> String -> [Package] -> ACYCache
           -> [CTypeExpr] -> ErrorLogger (ACYCache, Bool)
typesEqual texp dirA dirB deps acyCache checked =
  maybe (fail $ "typesEqual not called on type constructor: " ++ show texp)
        (return . fst)
        (tconsArgsOfType texp) >>= \n -> let (mod,_) = n in
  if texp `elem` checked
    then return (acyCache, True)
    else readCached dirA deps acyCache mod >>= \(acy',modA) ->
         readCached dirB deps acy' mod >>= \(acy'', modB) ->
         let typeA = findType n modA
             typeB = findType n modB
         in typesEqual' typeA typeB acy''
 where
  findType n m = case predefinedType n of
    Nothing -> find ((== n) . typeName) $ filter isTypePublic $ types m
    Just ty -> Just ty

  typesEqual' :: Maybe CTypeDecl -> Maybe CTypeDecl -> ACYCache
              -> ErrorLogger (ACYCache, Bool)
  typesEqual' (Just (CType n1 v1 tvs1 cs1 _)) (Just (CType n2 v2 tvs2 cs2 _))
              acy =
    if n1 == n2 && v1 == v2 && tvs1 == tvs2 && cs1 == cs2
      then foldM (\(a, r) (c1, c2) -> consEqual a c1 c2 >>= \(a', r') ->
           return (a', r && r')) (acy, True) (zip cs1 cs2)
      else return (acy, False)
  typesEqual' (Just (CTypeSyn n1 v1 tvs1 e1))
              (Just (CTypeSyn n2 v2 tvs2 e2)) acy =
    if n1 == n2 && v1 == v2 && tvs1 == tvs2 && e1 == e2
      then if isConsType e1
        then typesEqual e1 dirA dirB deps acy (texp:checked)
        else return (acy, True)
      else return (acy, False)
  typesEqual' (Just (CNewType n1 v1 tvs1 c1 _))
              (Just (CNewType n2 v2 tvs2 c2 _)) acy =
    if n1 == n2 && v1 == v2 && tvs1 == tvs2 && c1 == c2
      then consEqual acy c1 c2
      else return (acy, False)
  typesEqual' (Just (CType _ _ _ _ _)) (Just (CTypeSyn _ _ _ _)) acy =
    return (acy, False)
  typesEqual' (Just (CType _ _ _ _ _)) (Just (CNewType _ _ _ _ _)) acy =
    return (acy, False)
  typesEqual' (Just (CTypeSyn _ _ _ _)) (Just (CType _ _ _ _ _)) acy =
    return (acy, False)
  typesEqual' (Just (CTypeSyn _ _ _ _)) (Just (CNewType _ _ _ _ _)) acy =
    return (acy, False)
  typesEqual' (Just (CNewType _ _ _ _ _)) (Just (CType _ _ _ _ _)) acy =
    return (acy, False)
  typesEqual' (Just (CNewType _ _ _ _ _)) (Just (CTypeSyn _ _ _ _)) acy =
    return (acy, False)
  typesEqual' Nothing (Just _) acy = return (acy, False)
  typesEqual' (Just _) Nothing acy = return (acy, False)
  typesEqual' Nothing  Nothing acy = return (acy, False)

  consEqual :: ACYCache -> CConsDecl -> CConsDecl
            -> ErrorLogger (ACYCache, Bool)
  consEqual acy (CCons _ _ es1) (CCons _ _ es2) =
    foldM esEqual (acy, True) (zip es1 es2)
   where
    esEqual (a, r) (e1, e2) = if e1 == e2
      then if isConsType e1
        then typesEqual e1 dirA dirB deps a (texp:checked)
        else return (acy, r)
      else return (acy, False)
  consEqual acy (CRecord _ _ fs1) (CRecord _ _ fs2) =
    foldM fEqual (acy, True) (zip fs1 fs2)
   where
    fEqual (a, r) (f1@(CField _ _ e1), f2@(CField _ _ _)) = if f1 == f2
      then if isConsType e1
        then typesEqual e1 dirA dirB deps a (texp:checked)
        else return (acy, r)
      else return (acy, False)
  consEqual acy (CCons _ _ _) (CRecord _ _ _) = return (acy, False)
  consEqual acy (CRecord _ _ _) (CCons _ _ _) = return (acy, False)

isTypePublic :: CTypeDecl -> Bool
isTypePublic (CType _ v _ _ _)    = v == Public
isTypePublic (CTypeSyn _ v _ _)   = v == Public
isTypePublic (CNewType _ v _ _ _) = v == Public

isConsType :: CTypeExpr -> Bool
isConsType (CTCons _) = True
isConsType (CTVar _) = False
isConsType (CFuncType _ _) = False
isConsType (CTApply t _) = isConsType t

------------------------------------------------------------------------------
--- Reads a module in AbstractCurry form.
readCached :: String -> [Package] -> ACYCache -> String
           -> ErrorLogger (ACYCache, CurryProg)
readCached dir deps acyCache mod = case findModuleDir dir mod acyCache of
  Just  p -> return (acyCache, p)
  Nothing -> do prog <- readAbstractCurryFromDeps dir deps mod
                return (addModuleDir dir mod prog acyCache, prog)

--- Reads all modules of the given package and finds all public functions
--- in all of those modules.
---
--- @param dirA the directory where copy A of the package is stored
--- @param dirB the directory where copy B of the package is stored
--- @param pkg the package
--- @param deps a list of package dependencies
--- @param mods the list of modules to search for public functions
findAllFunctions :: String -> String -> Package -> [Package] -> ACYCache
                 -> [String] -> ErrorLogger (ACYCache, [CFuncDecl])
findAllFunctions dirA dirB _ deps acyCache mods =
  logDebug ("Finding public functions of modules: " ++ intercalate "," mods) >>
  logDebug ("in package directories " ++ dirA ++ " and " ++ dirB) >>
  foldM findForMod (acyCache, []) mods >>=
  \(a, fs) -> return (a, nub fs)
 where
  findForMod (acy,fdecls) mod =
    readCached dirA deps acy mod >>= \(_, progA) ->
    readCached dirB deps acy mod >>= \(acy'', progB) ->
    let funcsA = filter isPublic $ functions progA
        funcsB = filter isPublic $ functions progB
    in return (acy'', fdecls ++ nubBy (\a b -> funcName a == funcName b)
                                         (funcsA ++ funcsB))

--- Checks whether a function is public.
isPublic :: CFuncDecl -> Bool
isPublic (CFunc _ _ Public _ _) = True
isPublic (CFunc _ _ Private _ _) = False
isPublic (CmtFunc _ _ _ Public _ _) = True
isPublic (CmtFunc _ _ _ Private _ _) = False

--- Prepares two packages from the global package cache in two versions for
--- comparison by copying them to the temporary directory and building renamed
--- versions.
---
--- @param cfg the CPM configuration
--- @param repo the package repository
--- @param gc the global package cache
--- @param nameA the name of the first package
--- @param verA the version of the first package
--- @param nameB the name of the second package
--- @param verB the version of the second package
preparePackages :: Config
                -> Repository
                -> GC.GlobalCache
                -> String
                -> Version
                -> String
                -> Version
                -> ErrorLogger ComparisonInfo
preparePackages cfg repo gc nameA verA nameB verB =
  GC.tryFindPackage gc nameA verA >>= \pkgA ->
  findPackageDir cfg pkgA >>= \dirA ->
  GC.tryFindPackage gc nameB verB >>= \pkgB ->
  findPackageDir cfg pkgB >>= \dirB ->
  preparePackageDirs cfg repo gc dirA dirB

--- Prepares two package, one from a directory and one from the global package
--- cache. Copies them to a temporary directory and builds renamed versions of
--- the packages and all dependencies.
---
--- @param cfg the CPM configuration
--- @param repo the package repository
--- @param gc the global package cache
--- @param dirA the directory for the first package
--- @param nameB the name of the second package
--- @param verB the version of the second package
preparePackageAndDir :: Config 
                     -> Repository 
                     -> GC.GlobalCache 
                     -> String 
                     -> String 
                     -> Version 
                     -> ErrorLogger ComparisonInfo
preparePackageAndDir cfg repo gc dirA nameB verB =
  GC.tryFindPackage gc nameB verB >>= \pkgB ->
  findPackageDir cfg pkgB >>= \dirB ->
  preparePackageDirs cfg repo gc dirA dirB

--- Prepares two packages from two directories for comparison. Copies the 
--- package files to a temporary directory and creates renamed version of the
--- packages and their dependencies.
---
--- @param cfg the CPM configuration
--- @param repo the package repository
--- @param gc the global package cache
--- @param dirA the directory containing the first package
--- @param dirB the directory containing the second package
preparePackageDirs :: Config
                   -> Repository
                   -> GC.GlobalCache
                   -> String
                   -> String
                   -> ErrorLogger ComparisonInfo
preparePackageDirs cfg repo gc dirA dirB = do
  baseTmp <- liftIOEL $ createBaseTemp
  specA <- loadPackageSpec dirA
  specB <- loadPackageSpec dirB
  let versionPrefixA = versionPrefix specA
  let versionPrefixB = versionPrefix specB
  let copyDirA       = baseTmp </> ("src_" ++ versionPrefixA)
  let copyDirB       = baseTmp </> ("src_" ++ versionPrefixB)
  let destDirA       = baseTmp </> ("dest_" ++ versionPrefixA)
  let destDirB       = baseTmp </> ("dest_" ++ versionPrefixB)
  logDebug ("Copying " ++ packageId specA ++
             " from " ++ dirA ++ " into " ++ copyDirA)
  logDebug ("and transforming it into " ++ destDirA)
  logDebug ("Copying " ++ packageId specB ++
             " from " ++ dirB ++ " into " ++ copyDirB)
  logDebug ("and transforming it into " ++ destDirB)
  modMapA <- copyAndPrefixPackage cfg repo gc dirA versionPrefixA
                                        copyDirA destDirA
  modMapB <- copyAndPrefixPackage cfg repo gc dirB versionPrefixB
                                        copyDirB destDirB
  return $ ComparisonInfo
    { infPackageA   = specA
    , infPackageB   = specB
    , infDirA       = destDirA
    , infDirB       = destDirB
    , infSourceDirA = copyDirA
    , infSourceDirB = copyDirB
    , infPrefixA    = versionPrefixA
    , infPrefixB    = versionPrefixB
    , infModMapA    = modMapA
    , infModMapB    = modMapB }

versionPrefix :: Package -> String
versionPrefix pkg = "V_" ++ (showVersion' $ version pkg)

--- Copies a package from a directory to the temporary directory and creates
--- another copy of the package with all its modules and the modules of its
--- dependencies prefixed with the given string.
---
--- @param cfg the CPM configuration
--- @param repo the package repository
--- @param gc the global package cache
--- @param pkgDir the package directory to copy from
--- @param prefix the prefix for the modules
--- @param tmpDir the temporary directory to copy the files to
--- @param srcDir the temporary directory where the source package is copied
--- @param destDir the temporary directory where the prefixed copy is written
copyAndPrefixPackage :: Config
                     -> Repository
                     -> GC.GlobalCache
                     -> String
                     -> String
                     -> String
                     -> String
                     -> ErrorLogger [(String, String)]
copyAndPrefixPackage cfg repo gc pkgDir prefix srcDir destDir = do
  liftIOEL $ copyDirectory pkgDir srcDir
  liftIOEL $ createDirectory destDir
  prefixPackageAndDeps cfg repo gc srcDir (prefix ++ "_") destDir

showVersion' :: Version -> String
showVersion' (maj, min, pat, Nothing) =
  intercalate "_" [show maj, show min, show pat]
showVersion' (maj, min, pat, Just pre) =
  intercalate "_" [show maj, show min, show pat, pre]

--- Tries to find the package directory in the global package cache.
findPackageDir :: Config -> Package -> ErrorLogger String
findPackageDir cfg pkg = do
  exists <- liftIOEL $ doesDirectoryExist srcDir
  if not exists
    then fail $ "Package " ++ (packageId pkg) ++ " not installed"
    else return srcDir
 where
  srcDir = GC.installedPackageDir cfg pkg
