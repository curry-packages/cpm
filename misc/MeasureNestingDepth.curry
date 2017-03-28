module MeasureNestingDepth where

import Directory
import Debug
import List
import Distribution
import FilePath
import AbstractCurry.Types
import AbstractCurry.Select
import AbstractCurry.Files

sysLibPath = case curryCompiler of
  "pakcs" -> installDir </> "lib"
  "kics"  -> installDir </> "src" </> "lib"
  "kics2" -> installDir </> "lib"
  _       -> error "unknown curryCompiler"

main = do
  putStrLn "Reading all standard libary modules..."
  mods <- readAllStandardModules
  putStrLn "Calculating depths..."
  depths <- return $ concatMap (typeDepths mods . snd) mods
  orderedDepths <- return $ sortBy (\a b -> snd a >= snd b) depths
  putStrLn $ "The following are the 40 type with the deepest level of nesting " ++
    "out of all " ++ (show $ length depths) ++ " types."
  putStrLn $ show $ take 40 orderedDepths

typeDepths :: [(String, CurryProg)] -> CurryProg -> [(QName, Int)]
typeDepths progs (CurryProg _ _ ts _ _) = zip (map typeName ts) (map (typeDepth [] progs) ts)

typeDepth :: [QName] -> [(String, CurryProg)] -> CTypeDecl -> Int
typeDepth seen progs (CType n _ _ cs) = if n `elem` seen 
  then 0
  else 1 + (foldl max 0 $ map (typeConsDepth (n:seen) progs) cs)
typeDepth seen progs (CTypeSyn n _ _ e) = if n `elem` seen
  then 0
  else 1 + (typeExprDepth (n:seen) progs e)
typeDepth seen progs (CNewType n _ _ c) = if n `elem` seen
  then 0
  else 1 + (typeConsDepth (n:seen) progs c)

typeConsDepth :: [QName] -> [(String, CurryProg)] -> CConsDecl -> Int
typeConsDepth seen progs (CCons _ _ es) = foldl max 0 $ map (typeExprDepth seen progs) es
typeConsDepth seen progs (CRecord _ _ fs) = foldl max 0 $ map fieldDepth fs
 where
  fieldDepth (CField _ _ e) = typeExprDepth seen progs e

typeExprDepth :: [QName] -> [(String, CurryProg)] -> CTypeExpr -> Int
typeExprDepth _ _ (CTVar _) = 0
typeExprDepth seen progs (CFuncType e1 e2) = max (typeExprDepth seen progs e1) (typeExprDepth seen progs e2)
typeExprDepth seen progs e@(CTCons n@(mod, _) es) = typeDepth seen progs ity
 where
  ty = case predefinedType n of
    Nothing -> case lookup mod progs of
      Nothing -> error $ "Module " ++ mod ++ " not found"
      Just  p -> case find ((== n) . typeName) $ types p of
        Nothing -> error $ "Type " ++ (show n) ++ " not found in module " ++ mod
        Just  x -> x
    Just x -> x
  ity = instantiateType e ty

instantiateType :: CTypeExpr -> CTypeDecl -> CTypeDecl
instantiateType (CTCons _ es) (CType nt v vs cs) = 
  CType nt v vs ics
 where
  ics = map (instantiateConstructor $ zip vs es) cs
instantiateType _ a@(CTypeSyn _ _ _ _) = a
instantiateType _ a@(CNewType _ _ _ _) = a

instantiateConstructor :: [(CTVarIName, CTypeExpr)] -> CConsDecl -> CConsDecl
instantiateConstructor vs (CCons n v es) = CCons n v $ map (\e -> foldl (flip replaceVar) e vs) es
instantiateConstructor vs (CRecord n v fs) = CRecord n v $ map instantiateField fs
 where
  instantiateField (CField n' v' e) = CField n' v' $ foldl (flip replaceVar) e vs

replaceVar :: (CTVarIName, CTypeExpr) -> CTypeExpr -> CTypeExpr
replaceVar (b, e) (CTVar a) | a == b    = e
                            | otherwise = CTVar a
replaceVar v (CFuncType e1 e2) = CFuncType (replaceVar v e1) (replaceVar v e2)
replaceVar v (CTCons n es) = CTCons n $ map (replaceVar v) es

readAllStandardModules :: IO [(String, CurryProg)]
readAllStandardModules = do
  entries <- getDirectoryContents sysLibPath
  mods <- mapIO (readMods "" sysLibPath) $ filter (not . isIrrelevant) entries
  return $ concat mods
 where
  readMods :: String -> String -> String -> IO [(String, CurryProg)]
  readMods prefix base f = do
    isDir <- doesDirectoryExist $ base </> f
    if isDir
      then do
        entries <- getDirectoryContents $ base </> f
        mods <- mapIO (readMods (prefixIt prefix f) $ base </> f) $ filter (not . isIrrelevant) entries
        return $ concat mods
      else do
        callFrontendWithParams ACY (setQuiet True defaultParams) $ stripCurrySuffix $ prefixIt prefix f
        prog <- readCurry $ prefixIt prefix f
        return [(stripCurrySuffix $ prefixIt prefix f, prog)]
  prefixIt prefix m | prefix == "" = m
                    | otherwise    = prefix ++ "." ++ m

isIrrelevant :: String -> Bool
isIrrelevant d = "." `isPrefixOf` d || (takeExtensions d /= "" && takeExtensions d /= ".curry") || d == "dist" || d == "Makefile"

predefinedType :: (String, String) -> Maybe CTypeDecl
predefinedType x = case x of
  ("Prelude", "[]") -> Just $ CType ("Prelude", "[]") Public [(0, "a")] [
      CCons ("Prelude", "[]") Public []
    , CCons ("Prelude", ":") Public [CTVar (0, "a"), CTCons ("Prelude", "[]") [CTVar (0, "a")]]]
  ("Prelude", "(,)") -> Just $ CType ("Prelude", "(,)") Public [(0, "a"), (1, "b")] [
    CCons ("Prelude", "(,)") Public [CTVar (0, "a"), CTVar (1, "b")]]
  ("Prelude", "(,,)") -> Just $ CType ("Prelude", "(,,)") Public [(0, "a"), (1, "b"), (2, "c")] [
    CCons ("Prelude", "(,,)") Public [CTVar (0, "a"), CTVar (1, "b"), CTVar (2, "c")]]
  ("Prelude", "(,,,)") -> Just $ CType ("Prelude", "(,,,)") Public [(0, "a"), (1, "b"), (2, "c"), (3, "d")] [
    CCons ("Prelude", "(,,,)") Public [CTVar (0, "a"), CTVar (1, "b"), CTVar (2, "c"), CTVar (3, "d")]]
  ("Prelude", "(,,,,)") -> Just $ CType ("Prelude", "(,,,,)") Public [(0, "a"), (1, "b"), (2, "c"), (3, "d"), (4, "e")] [
    CCons ("Prelude", "(,,,,)") Public [CTVar (0, "a"), CTVar (1, "b"), CTVar (2, "c"), CTVar (3, "d"), CTVar (4, "e")]]
  ("Prelude", "()") -> Just $ CType ("Prelude", "()") Public [] [CCons ("Prelude", "()") Public []]
  _ -> Nothing
