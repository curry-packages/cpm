--------------------------------------------------------------------------------
--- This module contains the performance test program, currently covering API
--- and behavior diff as well as resolution performance.
--------------------------------------------------------------------------------

module PerformanceTest where

import ReadShowTerm
import System.IO
import System.Directory
import System.FilePath ((</>))
import System.Process
import System.IOExts
import Debug.Trace
import Data.List
import Data.Maybe
import Data.Function
import Data.Either

import AbstractCurry.Build
import AbstractCurry.Types hiding (version)
import AbstractCurry.Pretty
import Debug.Profile
import JSON.Pretty

import CPM.LookupSet
import CPM.ErrorLogger
import CPM.FileUtil (recreateDirectory)
import CPM.Package
import CPM.Resolution
import CPM.Diff.API hiding (getBaseTemp)
import CPM.Diff.Behavior
import CPM.Config
import CPM.Repository (Repository, emptyRepository)
import qualified CPM.PackageCache.Global as GC
import OptParse

--- Possible performance tests.
data Command = BehaviorDiff | APIDiff | Resolution | CountDeps

--- Command line options.
data Options = Options
  { optCommand :: Command
  , bdTypeNesting :: Int
  , bdNumberOfFuncs :: Int
  , adNumberOfEach :: Int
  , resRepoPath :: Maybe String
  , resPackages :: String
  , cdPackage :: String }

defaultOptions :: Options
defaultOptions = Options {
    optCommand = BehaviorDiff
  , bdTypeNesting = 10
  , bdNumberOfFuncs = 10
  , adNumberOfEach = 10
  , resRepoPath = Nothing
  , resPackages = ""
  , cdPackage = ""
  }

optionParser :: ParseSpec (Options -> Options)
optionParser = optParser
  (   commands (metavar "COMMAND")
        (   command "behavior" (help "Run behavior diff performance test.") (\a -> a { optCommand = BehaviorDiff })
              (   option (\s a -> a { bdTypeNesting = readInt s })
                    (  metavar "TYPE-NESTING"
                    <> help "Depth of nested type"
                    <> long "type-nesting"
                    <> short "t" )
              <.> option (\s a -> a { bdNumberOfFuncs = readInt s })
                    (  metavar "NUMBER-OF-FUNCS"
                    <> help "Number of functions to generate"
                    <> long "functions"
                    <> short "f" ) )
        <|> command "api" (help "Run API diff performance test.") (\a -> a { optCommand = APIDiff })
              (   option (\s a -> a { adNumberOfEach = readInt s })
                    (  metavar "NUMBER-OF-EACH"
                    <> help "Number of added, removed, changed functions and types to generate."
                    <> long "number"
                    <> short "n") )
        <|> command "resolution" (help "Run resolution performance test.") (\a -> a { optCommand = Resolution })
              (   option (\s a -> a { resRepoPath = Just s })
                    (  metavar "REPO-PATH"
                    <> help "A path to a package repository to read. Repository will be read from packages.term if not specified. If specified, repository will be read from this path and then written to packages.term. Note that reading large repositories is not supported on PAKCS."
                    <> long "repo-path"
                    <> short "r")
              <.> option (\s a -> a { resPackages = s })
                    (  metavar "PACKAGES"
                    <> help "The package versions to resolve. Expects multiple package ids, i.e., package-version, separated by commas. Pre-release versions are not supported."
                    <> long "packages"
                    <> short "p") )
        <|> command "countdeps" (help "Count dependencies of a package.") (\a -> a { optCommand = CountDeps })
              ( arg (\s a -> a { cdPackage = s })
                  (  metavar "PACKAGE"
                  <> help "The package to count dependencies for." ) ) ) )
main :: IO ()
main = do
  args <- getArgs
  -- The shell script that is used to execute CPM.PerformanceTest always adds
  -- the command line options `+RTS -T -RTS`, since they are needed to enable
  -- profiling for applications compiled with KiCS2. They are automatically on
  -- KiCS2, but will still be there when using PAKCS, confusing OptParse. So we
  -- remove them from the list if they're present.
  args' <- return $ filter (\a -> a /= "+RTS" && a /= "-T" && a /= "-RTS") args
  parseResult <- return $ parse (intercalate " " args') optionParser "perftest"
  case parseResult of
    Left err -> putStrLn err >> exitWith 1
    Right  r -> let opts = foldl (flip apply) defaultOptions r in
      case optCommand opts of
        BehaviorDiff -> behaviorDiffPerformance opts
        APIDiff -> apiDiffPerformance opts
        Resolution -> resolutionPerformance opts
        CountDeps -> countDeps opts

behaviorDiffPerformance :: Options -> IO ()
behaviorDiffPerformance o = do
  putStrLn $ "Running behavior diff performance test with " ++ (show $ bdNumberOfFuncs o) ++ " functions and a type nesting depth of " ++ (show $ bdTypeNesting o)
  types <- return $ genNestedType (bdTypeNesting o)
  funcs <- return $ map (genCompareFunc (bdTypeNesting o)) [1..(bdNumberOfFuncs o)]
  prog <- return $ CurryProg "Sample" [] types funcs []
  recreateDirectory "/tmp/verA"
  recreateDirectory "/tmp/verA/src"
  recreateDirectory "/tmp/verB"
  recreateDirectory "/tmp/verB/src"
  writeFile "/tmp/verA/src/Sample.curry" $ showCProg prog
  writeFile "/tmp/verB/src/Sample.curry" $ showCProg prog
  writeFile "/tmp/verA/package.json" (ppJSON $ packageSpecToJSON samplePackageA)
  writeFile "/tmp/verB/package.json" (ppJSON $ packageSpecToJSON samplePackageB)
  profileTime genTestProgram
  profileTime genTestProgram
  profileTime genTestProgram
  profileTime genTestProgram
  profileTime genTestProgram
  putStrLn "DONE"

genTestProgram :: ErrorLogger ()
genTestProgram = preparePackageDirs defaultConfig emptyRepository GC.emptyCache "/tmp/verA" "/tmp/verB" |>=
    \info -> findFunctionsToCompare defaultConfig emptyRepository GC.emptyCache (infSourceDirA info) (infSourceDirB info) False Nothing |>=
    \(acyCache, loadpath, funcs, _) ->
    genCurryCheckProgram defaultConfig emptyRepository GC.emptyCache funcs
                         info True acyCache loadpath |>
    return ()

apiDiffPerformance :: Options -> IO ()
apiDiffPerformance o = do
  putStrLn $ "Running API diff performance test with " ++ (show n) ++ " functions"
  (prog1, prog2) <- return $ genDiffProgs n n n n n n
  recreateDirectory "/tmp/verA"
  recreateDirectory "/tmp/verA/src"
  recreateDirectory "/tmp/verB"
  recreateDirectory "/tmp/verB/src"
  writeFile "/tmp/verA/src/Sample.curry" (showCProg prog1)
  writeFile "/tmp/verB/src/Sample.curry" (showCProg prog2)
  writeFile "/tmp/verA/package.json" (ppJSON $ packageSpecToJSON samplePackageA)
  writeFile "/tmp/verB/package.json" (ppJSON $ packageSpecToJSON samplePackageB)
  putStrLn "Files written, now starting test."
  profileTime (id $!! compareModulesInDirs defaultConfig emptyRepository GC.emptyCache "/tmp/verA" "/tmp/verB" Nothing)
  profileTime (id $!! compareModulesInDirs defaultConfig emptyRepository GC.emptyCache "/tmp/verA" "/tmp/verB" Nothing)
  profileTime (id $!! compareModulesInDirs defaultConfig emptyRepository GC.emptyCache "/tmp/verA" "/tmp/verB" Nothing)
  profileTime (id $!! compareModulesInDirs defaultConfig emptyRepository GC.emptyCache "/tmp/verA" "/tmp/verB" Nothing)
  profileTime (id $!! compareModulesInDirs defaultConfig emptyRepository GC.emptyCache "/tmp/verA" "/tmp/verB" Nothing)
  putStrLn "DONE"
 where
  n = adNumberOfEach o

readPackageSpecs :: Options -> IO LookupSet
readPackageSpecs o = case resRepoPath o of
  Nothing -> do
    putStrLn "About to read packages.term..."
    readQTermFile "packages.term" >>=
      \l -> return $!! addPackages emptySet l FromRepository
  Just  p -> do
    putStrLn $ "About to read the index from '" ++ p ++ "' ..."
    ls <- profileTime $ readLS p
    putStrLn "Writing the index to packages.term..."
    writeQTermFile "packages.term" (id $!! allPackages ls)
    return ls

resolutionPerformance :: Options -> IO ()
resolutionPerformance o = do
  putStrLn "Reading package specifications..."
  ls <- readPackageSpecs o
  pkgs <- return $ map (fromJust . (uncurry $ findVersion ls)) $ parsePkgs $ resPackages o
  putStrLn $ "Running resolution algorithm on " ++ intercalate ", " (map packageId pkgs)
  forIO pkgs $ \pkg -> do
    putStrLn $ "Resolving '" ++ packageId pkg ++ "'"
    profileTime (putStrLn $ showResult $ resolve pkg ls)
    profileTime (putStrLn $ showResult $ resolve pkg ls)
    profileTime (putStrLn $ showResult $ resolve pkg ls)
    profileTime (putStrLn $ showResult $ resolve pkg ls)
    profileTime (putStrLn $ showResult $ resolve pkg ls)
  return ()
 where
  parsePkgs s = map parsePkg $ splitOn "," s

parsePkg :: String -> (String, Version)
parsePkg s =
  let
    split = splitOn "-" s
    pkgName = intercalate "-" $ init split
    pkgVer = last split
  in case readVersion pkgVer of
    Nothing -> error $ "Could not read version for  '" ++ s ++ "'"
    Just  v -> (pkgName, v)

countDeps :: Options -> IO ()
countDeps o = do
  putStrLn "Reading package specifications..."
  ls <- readPackageSpecs o
  pkg <- return $ fromJust $ uncurry (findVersion ls) $ parsePkg (cdPackage o)
  transDeps <- return $ transitiveDependencies ls pkg
  verCount <- return $ foldl (flip $ (+) . length . (findAllVersions' ls)) 0 transDeps
  putStrLn $ packageId pkg ++ " has " ++ (show $ length transDeps)
    ++ " dependencies with " ++ show verCount ++ " versions"
 where
  findAllVersions' ls p = findAllVersions ls p True

readLS :: String -> IO LookupSet
readLS path = do
  pkgDirs <- getDirectoryContents path
  pkgPaths <- return $ map (path </>) $ filter (not . isPrefixOf ".") pkgDirs
  verDirs <- mapIO getDirectoryContents pkgPaths
  verPaths <- return $ concat $ map (\(d, p) -> map (d </>) (filter (not . isPrefixOf ".") p)) $ zip pkgPaths verDirs
  specPaths <- return $ map (</> "package.json") verPaths
  specs <- mapIO (readPackageSpecIO specPaths) specPaths
  return $!! addPackages emptySet (rights specs) FromRepository
    where
      readPackageSpecIO _ p = do
        s <- readCompleteFile p
        return $ readPackageSpec s

extractComponents :: [(a, b)] -> ([a], [b])
extractComponents ts = (as, bs)
 where
  as = map fst ts
  bs = map snd ts

data APIDiffType = Added
                 | Removed
                 | Changed

samplePackageA :: Package
samplePackageA = Package {
    name = "sample"
  , version = (0, 0, 1, Nothing)
  , author = "author"
  , synopsis = "JSON library for Curry"
  , dependencies = []
  , maintainer = Nothing
  , description = Nothing
  , license = Nothing
  , licenseFile = Nothing
  , copyright = Nothing
  , homepage = Nothing
  , bugReports = Nothing
  , repository = Nothing
  , compilerCompatibility = []
  , source = Nothing
  , exportedModules = ["Sample"]
  , executableSpec  = []
  , testSuite       = Nothing
  }

samplePackageB :: Package
samplePackageB = samplePackageA {
  version = (0, 0, 2, Nothing) }

stringType :: CTypeExpr
stringType = CTCons ("Prelude", "String") []

genDiffFunc :: APIDiffType -> Int -> (Maybe CFuncDecl, Maybe CFuncDecl)
genDiffFunc Added n = (Just f, Nothing)
 where
  f = cfunc ("Sample", "f" ++ (show n)) 1 Public (CFuncType stringType stringType) [
    simpleRule (pVars 1) (toVar 0) ]
genDiffFunc Removed n = (Nothing, Just f)
 where
  f = cfunc ("Sample", "f" ++ (show n)) 1 Public (CFuncType stringType stringType) [
    simpleRule (pVars 1) (toVar 0) ]
genDiffFunc Changed n = (Just f1, Just f2)
 where
  f1 = cfunc ("Sample", "f" ++ (show n)) 1 Public (CFuncType stringType stringType) [
    simpleRule (pVars 1) (toVar 0) ]
  f2 = cfunc ("Sample", "f" ++ (show n)) 2 Public (CFuncType stringType (CFuncType stringType stringType)) [
    simpleRule (pVars 2) (toVar 1) ]

genDiffFuncs :: Int -> Int -> APIDiffType -> ([CFuncDecl], [CFuncDecl])
genDiffFuncs s n t = both catMaybes $ extractComponents $ map (genDiffFunc t) (enumFromTo s n)

allCons :: Int -> [CConsDecl]
allCons n = [ CCons ("Sample", "Cons1" ++ (show n)) Public [stringType]
            , CCons ("Sample", "Cons2" ++ (show n)) Public [stringType]
            , CCons ("Sample", "Cons3" ++ (show n)) Public [stringType] ]

notAllCons :: Int -> [CConsDecl]
notAllCons n = [ CCons ("Sample", "Cons1" ++ (show n)) Public [stringType]
               , CCons ("Sample", "Cons2" ++ (show n)) Public [stringType] ]

genDiffType :: APIDiffType -> Int -> (Maybe CTypeDecl, Maybe CTypeDecl)
genDiffType Added n = (Just t, Nothing)
 where
  t = CType ("Sample", "Type" ++ (show n)) Public [] (allCons n)
genDiffType Removed n = (Nothing, Just t)
 where
  t = CType ("Sample", "Type" ++ (show n)) Public [] (allCons n)
genDiffType Changed n = (Just t1, Just t2)
 where
  t1 = CType ("Sample", "Type" ++ (show n)) Public [] (allCons n)
  t2 = CType ("Sample", "Type" ++ (show n)) Public [] (notAllCons n)

genDiffTypes :: Int -> Int -> APIDiffType -> ([CTypeDecl], [CTypeDecl])
genDiffTypes s n t = both catMaybes $ extractComponents $ map (genDiffType t) (enumFromTo s n)

genDiffProgs :: Int -> Int -> Int -> Int -> Int -> Int -> (CurryProg, CurryProg)
genDiffProgs nfsAdded nfsRemoved nfsChanged ntsAdded ntsRemoved ntsChanged =
  (prog1, prog2)
 where
  fromfsAdded = 1
  tofsAdded = nfsAdded
  fromfsRemoved = tofsAdded + 1
  tofsRemoved = tofsAdded + nfsRemoved
  fromfsChanged = tofsRemoved + 1
  tofsChanged = tofsRemoved + nfsChanged
  fromtsAdded = tofsChanged + 1
  totsAdded = tofsChanged + ntsAdded
  fromtsRemoved = totsAdded + 1
  totsRemoved = totsAdded + ntsRemoved
  fromtsChanged = totsRemoved + 1
  totsChanged = totsRemoved + ntsChanged
  (fsAdded1, fsAdded2) = genDiffFuncs fromfsAdded tofsAdded Added
  (fsRemoved1, fsRemoved2) = genDiffFuncs fromfsRemoved tofsRemoved Removed
  (fsChanged1, fsChanged2) = genDiffFuncs fromfsChanged tofsChanged Changed
  (tsAdded1, tsAdded2) = genDiffTypes fromtsAdded totsAdded Added
  (tsRemoved1, tsRemoved2) = genDiffTypes fromtsRemoved totsRemoved Removed
  (tsChanged1, tsChanged2) = genDiffTypes fromtsChanged totsChanged Changed
  prog1 = CurryProg "Sample" [] (tsAdded1 ++ tsRemoved1 ++ tsChanged1) (fsAdded1 ++ fsRemoved1 ++ fsChanged1) []
  prog2 = CurryProg "Sample" [] (tsAdded2 ++ tsRemoved2 ++ tsChanged2) (fsAdded2 ++ fsRemoved2 ++ fsChanged2) []

genNestedType :: Int -> [CTypeDecl]
genNestedType n | n == 0    = [CType ("Sample", "Nested0") Public [] [CCons ("Sample", "Nested0") Public [stringType]]]
                | otherwise = let t = (CType ("Sample", "Nested" ++ (show n)) Public [] [CCons ("Sample", "Nested" ++ (show n)) Public [CTCons ("Sample", "Nested" ++ (show (n - 1))) []]]) in
                  t:(genNestedType (n - 1))

genCompareFunc :: Int -> Int -> CFuncDecl
genCompareFunc tn n = cfunc ("Sample", "f" ++ (show n)) 1 Public (CFuncType (CTCons ("Sample", "Nested" ++ (show tn)) []) (CTCons ("Sample", "Nested" ++ (show tn)) [])) [
  simpleRule (pVars 1) (toVar 0) ]
