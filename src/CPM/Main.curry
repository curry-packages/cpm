--------------------------------------------------------------------------------
--- This is the main module of the Curry Package Manager.
--------------------------------------------------------------------------------

module CPM.Main ( main )
 where

import Curry.Compiler.Distribution ( installDir )
import Control.Monad       ( when, unless )
import Crypto.Hash         ( getHash )
import Data.Char           ( toLower )
import Data.List           ( (\\), delete, findIndex, groupBy, init
                           , intercalate, isInfixOf, isPrefixOf, isSuffixOf
                           , nub, replace, split, sortBy )
import Data.Maybe          ( isJust, isNothing )
import Data.Time           ( calendarTimeToString, getClockTime, toUTCTime )
import FlatCurry.Files     ( readFlatCurryInt )
import FlatCurry.Goodies   ( progImports )
import JSON.Data
import JSON.Parser         ( parseJSON )
import JSON.Pretty         ( ppJSON )
import Network.URL         ( string2urlencoded )
import System.Directory    ( doesFileExist, doesDirectoryExist
                           , copyFile, createDirectory, createDirectoryIfMissing
                           , getCurrentDirectory, getDirectoryContents
                           , getModificationTime
                           , renameFile, removeFile, setCurrentDirectory )
import System.FilePath     ( (</>), equalFilePath, joinPath, splitDirectories
                           , splitSearchPath, replaceExtension, takeExtension
                           , takeFileName, pathSeparator, isPathSeparator )
import System.Environment  ( getArgs, getEnv, setEnv, unsetEnv )
import System.Process      ( exitWith, getPID, system )
import System.IO           ( hFlush, stdout )
import System.IOExts       ( evalCmd, readCompleteFile, updateFile )

import Boxes               ( table, render )
import Data.GraphViz       ( showDotGraph, viewDotGraph )
import OptParse            ( parse, printUsage )
import System.CurryPath    ( addCurrySubdir, curryModulesInDirectory
                           , stripCurrySuffix, sysLibPath )
import System.Path         ( fileInPath, getFileInPath )
import Text.CSV            ( readCSV, showCSV, writeCSVFile )

import CPM.ErrorLogger
import CPM.Executables     ( checkRequiredExecutables, getCurlCmdOpts
                           , getCurryCheck, getCurryDoc )
import CPM.FileUtil ( cleanTempDir, getRealPath, ifFileExists, joinSearchPath
                    , safeReadFile, whenFileExists, writeFileIfNotExists
                    , inDirectory, recreateDirectory
                    , removeDirectoryComplete, copyDirectory, quote, tempDir )
import CPM.Config   ( Config (..)
                    , readConfigurationWith, showCompilerVersion
                    , showConfiguration )
import CPM.Options
import CPM.PackageCache.Global ( acquireAndInstallPackage
                               , GlobalCache, readGlobalCache, allPackages
                               , checkoutPackage
                               , installFromZip, installedPackageDir
                               , uninstallPackage, packageInstalled )
import CPM.Package
import CPM.Package.Helpers ( cleanPackage, getLocalPackageSpec
                           , renderPackageInfo, installPackageSourceTo )
import CPM.Resolution ( isCompatibleToCompiler, showResult, showShortResult
                      , dependenciesAsGraph )
import CPM.Repository ( Repository, findVersion, listPackages
                      , findAllVersions, findLatestVersion
                      , useUpdateHelp, searchPackages, cleanRepositoryCache
                      , readPackageFromRepository )
import CPM.Repository.Update ( addPackageToRepository, updateRepository )
import CPM.Repository.Select
import CPM.PackageCache.Runtime ( dependencyPathsSeparate, writePackageConfig )
import CPM.PackageCopy
import CPM.Diff.API as APIDiff
import qualified CPM.Diff.Behavior as BDiff
import CPM.ConfigPackage        ( packagePath, packageVersion )
import CPM.Helpers ( askYesNo )

-- Date of current version:
cpmDate :: String
cpmDate = "24/04/2025"

-- Banner of this tool:
cpmBanner :: String
cpmBanner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText =
    "Curry Package Manager <curry-lang.org/tools/cpm> (Version " ++
    packageVersion ++ ", " ++ cpmDate ++ ")"
  bannerLine = take (length bannerText) (repeat '-')


main :: IO ()
main = do
  args <- getArgs
  if "-V" `elem` args || "--version" `elem` args
    then putStrLn $ "Curry Package Manager, version " ++ packageVersion ++
                    " (" ++ cpmDate ++ ")"
    else do
      parseResult <- return $ parse (unwords args) (optionParser args) "cypm"
      case parseResult of
        Left err -> do putStrLn cpmBanner
                       putStrLn err
                       --putStrLn "(use option -h for usage information)"
                       exitWith 1
        Right  r -> case applyParse r of
          Left err   -> do putStrLn cpmBanner
                           --printUsage "cypm" 80 (optionParser args)
                           putStrLn err
                           exitWith 1
          Right opts -> runWithArgs opts

runWithArgs :: Options -> IO ()
runWithArgs opts = do
  ((ll, _), (msgs, result)) <-
    runErrorLogger' (optLogLevel opts) (optWithTime opts) $ do
      logDebug "Reading CPM configuration..."
      config <- readConfigurationWith (optDefConfig opts) >>= \c ->
       case c of
        Left err -> do logError $ "Error reading .cpmrc settings: " ++ err
                       liftIOEL $ exitWith 1
        Right c' -> return c'
      logDebug $ "Current configuration:\n" ++ showConfiguration config
      case optCommand opts of
        NoCommand   -> fail "NoCommand"
        Help        -> liftIOEL $ do
                         putStrLn cpmBanner
                         printUsage "cypm" 80 (optionParser [])
        ConfigCmd o -> configCmd    o config
        Update o    -> updateCmd    o config
        Compiler o  -> curryCmd     o config
        Exec o      -> execCmd      o config
        Doc  o      -> docCmd       o config
        Test o      -> testCmd      o config
        Uninstall o -> uninstallCmd o config
        Deps o      -> depsCmd      o config
        PkgInfo   o -> infoCmd      o config
        Link o      -> linkCmd      o config
        Add  o      -> addCmd       o config
        Init        -> initCmd        config
        New o       -> newCmd       o config
        List      o -> listCmd      o config
        Search    o -> searchCmd    o config
        Upgrade   o -> upgradeCmd   o config
        Diff      o -> diffCmd      o config
        Check     o -> checkCmd     o config
        Checkout  o -> checkoutCmd  o config
        Install   o -> installCmd   o config
        Upload    o -> uploadCmd    o config
        Clean     o -> cleanCmd     o config
  mapM (printLogEntry ll) msgs
  let allOk =  all (levelGte Info) (map logLevelOf msgs) &&
               either (\le -> levelGte Info (logLevelOf le))
                      (const True)
                      result
  exitWith $ if allOk then 0 else 1
 where runErrorLogger' a b c = runErrorLogger c a b

------------------------------------------------------------------------------
-- `config` command: show current CPM configuration
configCmd :: ConfigOptions -> Config -> ErrorLogger ()
configCmd opts cfg
  | configAll opts = do
      repo <- getBaseRepository cfg
      gc <- readGlobalCache cfg repo
      liftIOEL $ do
        putStrLn configS
        putStrLn "Installed packages:\n"
        putStrLn $ unwords . sortBy (<=) . map packageId . allPackages $ gc
  | otherwise = putStrLnELM configS
 where
  configS = unlines
              [cpmBanner, "Current configuration:", "", showConfiguration cfg]

------------------------------------------------------------------------------
-- `update` command:
updateCmd :: UpdateOptions -> Config -> ErrorLogger ()
updateCmd opts cfg = do
  let cfg' = cfg { packageIndexURLs = indexURLs opts ++ packageIndexURLs cfg }
  checkRequiredExecutables
  updateRepository cfg' (cleanCache opts) (downloadIndex opts)
                        (useRepoCache opts) (writeCSV opts)

------------------------------------------------------------------------------
-- `deps` command:
depsCmd :: DepsOptions -> Config -> ErrorLogger ()
depsCmd opts cfg = do
  specDir <- getLocalPackageSpec cfg "."
  pkg     <- loadPackageSpec specDir
  checkCompiler cfg pkg
  when (depsVSCode opts) $ -- set full path in VSCode settings?
    setVSCodeImportPath cfg specDir
  when (depsLangServer opts) $ -- set full path for language server?
    setLanguageServerImportPath cfg specDir
  if depsPath opts -- show CURRYPATH only?
    then getCurryLoadPath cfg specDir >>= putStrLnELM
    else unless (depsVSCode opts || depsLangServer opts) $ showDeps specDir
 where
  showDeps specDir = do
    let printFailure = putStrLnELM "Dependency resolution failed."
    result <- resolveDependencies cfg specDir
    when (depsGraph opts) $ -- show dot graph?
      maybe printFailure
            (putStrLnELM . showDotGraph)
            (dependenciesAsGraph result)
    when (depsView opts) $ -- view dot graph?
      maybe printFailure
            (liftIOEL . viewDotGraph)
            (dependenciesAsGraph result)
    unless (depsGraph opts || depsView opts) $ putStrLnELM $
      (if depsFull opts then showResult else showShortResult) result

------------------------------------------------------------------------------
-- `info` command:
infoCmd :: InfoOptions -> Config -> ErrorLogger ()
infoCmd (InfoOptions Nothing (Just _) _ _) _ =
  fail "Must specify package name"
infoCmd (InfoOptions Nothing Nothing allinfos plain) cfg = do
  specDir <- getLocalPackageSpec cfg "."
  p       <- loadPackageSpec specDir
  liftIOEL $ printInfo cfg allinfos plain p
infoCmd (InfoOptions (Just pkgname) Nothing allinfos plain) cfg = do
  pkgs <- getAllPackageVersions cfg pkgname False
  case pkgs of
    [] -> packageNotFoundFailure pkgname
    ps -> case filter (isCompatibleToCompiler cfg) ps of
           [] -> let lvers = showVersion (version (head ps))
                 in compatPackageNotFoundFailure cfg pkgname
                      ("Use 'info " ++ pkgname ++ " " ++ lvers ++
                       "' to print info about the latest version.")
           (rp:_) -> do p <- readPackageFromRepository cfg rp
                        liftIOEL $ printInfo cfg allinfos plain p
infoCmd (InfoOptions (Just pkgname) (Just v) allinfos plain) cfg = do
  mbpkg <- getPackageVersion cfg pkgname v
  case mbpkg of
    Nothing -> packageNotFoundFailure $ pkgname ++ "-" ++ showVersion v
    Just rp -> do p <- readPackageFromRepository cfg rp
                  liftIOEL $ printInfo cfg allinfos plain p

printInfo :: Config -> Bool -> Bool -> Package -> IO ()
printInfo cfg allinfos plain pkg = do
  isinstalled <- packageInstalled cfg pkg
  putStrLn $ renderPackageInfo allinfos plain isinstalled pkg


------------------------------------------------------------------------------
-- `check` command:
checkCmd :: CheckOptions -> Config -> ErrorLogger ()
checkCmd chkopts cfg = do
  specDir <- getLocalPackageSpec cfg "."
  pkg     <- loadPackageSpec specDir
  checkCompiler cfg pkg
  aspecdir  <- liftIOEL $ getRealPath specDir
  srcmods <- checkCompleteDependencies chkopts cfg aspecdir pkg
  getCurryCheck cfg >>= maybe
    (logInfo "No further source code checks...")
    (\cc -> do
       lvl <- getLogLevel
       let cmd = unwords $
                   [cc] ++ (if levelGte Debug lvl then [] else ["-q"]) ++
                   ["--noprop", "--nospec", "--nodet", "--noproof"] ++
                   srcmods
       when (chkSource chkopts) $ do
         logInfo "Checking all source modules of package..."
         execWithPkgDir (ExecOptions cmd) cfg aspecdir)

-- Check whether the source modules of the given package imports
-- only modules from the direct dependencies or the base libraries.
-- Additionally, warnings about unused packages are issued.
checkCompleteDependencies :: CheckOptions -> Config -> String -> Package
                          -> ErrorLogger [String]
checkCompleteDependencies chkopts cfg aspecdir pkg = do
  let deps    = map (\ (Dependency p _) -> p) (dependencies pkg)
      pkgsdir = aspecdir </> ".cpm" </> "packages"
  -- get the complete load path for this package:
  loadpath <- getCurryLoadPath cfg aspecdir >>= return . splitSearchPath
  aloadpath <- mapM (liftIOEL . getRealPath) loadpath
  -- filter load path w.r.t. the packages in the dependency list:
  let dloadpath = filter (isDepsDir pkgsdir deps) aloadpath
  logDebug $ unlines ("Source and dependency directories:" : dloadpath)
  allmods <- mapM (\d -> liftIOEL (curryModulesInDirectory d) >>=
                           return . map (\m -> (m,d)))
                  (dloadpath ++ sysLibPath)
               >>= return . concat
  logDebug $ "Source and dependency modules:\n" ++ unwords (map fst allmods)
  -- compute the names of all source modules:
  allsrcmods <- getSourceModulesOfPkg aspecdir pkg
  let realsrcmods = filter (isNotHierarchical allsrcmods) allsrcmods
  when (chkInfo chkopts) $ liftIOEL $ putStrLn $
    "Package source contains " ++ show (length realsrcmods) ++
    " Curry modules:\n" ++ unwords realsrcmods
  -- check all source modules:
  let currypath = joinSearchPath loadpath
  logDebug $ "CURRYPATH=" ++ currypath
  liftIOEL (setEnv "CURRYPATH" currypath)
  usedsrcdirs <- mapM (checkImports allmods) realsrcmods >>=
                   return . nub . concat
  let usedpkgs   = map (packageOfDepsDir pkgsdir)
                       (filter (pkgsdir `isPrefixOf`) usedsrcdirs)
      unusedpkgs = delete "base" deps \\ usedpkgs
  mapM_ (\p -> logInfo $ "Warning: Package dependency '" ++ p ++
                         "' not used in source code.")
        unusedpkgs
  return realsrcmods
 where
  isDepsDir pkgsdir depsnames dir =
    not (pkgsdir `isPrefixOf` dir) ||
    any (\d -> d `isPrefixOf` pkgversion &&
               isJust (readVersion (drop (length d + 1) pkgversion)))
        depsnames
   where
    pkgversion = splitDirectories (drop (length pkgsdir) dir) !! 1

  packageOfDepsDir pkgsdir dir =
    intercalate "-"
      (fst (break (\s -> not (null s) && isDigit (head s))
              (split (=='-')
                     (splitDirectories (drop (length pkgsdir) dir) !! 1))))

  -- Is some of the given modules `mods` a suffix of the module `m`?
  -- In this case, module `m` is probably not a hierarchical one.
  isNotHierarchical mods m = all (\n -> not (('.':n) `isSuffixOf` m)) mods

  checkImports allowedimports mname = do
    logInfo $ "Checking interface of " ++ mname
    imps <- liftIOEL (readFlatCurryInt mname) >>= return . progImports
    logDebug $ "Imports: " ++ unwords imps
    let addimports  = imps \\ map fst allowedimports
        usedsrcdirs = map (\m -> maybe [] (:[]) (lookup m allowedimports)) imps
    unless (null addimports) $ logError $
      "ILLEGAL IMPORTS (add to package dependencies!): " ++ unwords addimports
    return (nub (concat usedsrcdirs))

------------------------------------------------------------------------------
-- `checkout` command:
checkoutCmd :: CheckoutOptions -> Config -> ErrorLogger ()
checkoutCmd (CheckoutOptions pkgname Nothing pre codir) cfg = do
 repo <- getRepoForPackages cfg [pkgname]
 case findAllVersions repo pkgname pre of
  [] -> packageNotFoundFailure pkgname
  ps -> case filter (isCompatibleToCompiler cfg) ps of
    []    -> compatPackageNotFoundFailure cfg pkgname useUpdateHelp
    (p:_) -> do acquireAndInstallPackageWithDependencies cfg repo p
                checkoutPackage cfg p codir
checkoutCmd (CheckoutOptions pkgname (Just ver) _ codir) cfg = do
 repo <- getRepoForPackages cfg [pkgname]
 case findVersion repo pkgname ver of
  Nothing -> packageNotFoundFailure $ pkgname ++ "-" ++ showVersion ver
  Just  p -> do acquireAndInstallPackage cfg p
                checkoutPackage cfg p codir

------------------------------------------------------------------------------
-- `install` command:
installCmd :: InstallOptions -> Config -> ErrorLogger ()
installCmd (InstallOptions Nothing Nothing _ instexec False) cfg = do
  pkgdir <- getLocalPackageSpec cfg "."
  cleanCurryPathCache pkgdir
  (pkg,_) <- installLocalDependencies cfg pkgdir
  currypath <- getCurryLoadPath cfg pkgdir
  writePackageConfig cfg pkgdir pkg currypath
  when instexec $ installExecutable cfg pkg Nothing
-- Install executable only:
installCmd (InstallOptions mbexec Nothing _ _ True) cfg = do
  pkgdir <- getLocalPackageSpec cfg "."
  pkg    <- loadPackageSpec pkgdir
  installExecutable cfg pkg mbexec
installCmd (InstallOptions _ (Just _) _ _ True) _ =
  fail "Cannot use option '--exec'  for a specific package version"
installCmd (InstallOptions (Just pkg) vers pre _ False) cfg = do
  fileExists <- liftIOEL $ doesFileExist pkg
  if fileExists
    then installFromZip cfg pkg
    else installApp (CheckoutOptions pkg vers pre "") cfg
installCmd (InstallOptions Nothing (Just _) _ _ False) _ =
  fail "Must specify package name"

--- Installs the application (i.e., binary) provided by a package.
--- This is done by checking out the package into CPM's application packages
--- cache (default: $HOME/.cpm/app_packages, see APP_PACKAGE_PATH
--- in .cpmrc configuration file) and then install this package.
---
--- Internal note: the installed package should not be cleaned or removed
--- after the installation since its execution might refer (via the
--- config module) to some data stored in the package.
installApp :: CheckoutOptions -> Config -> ErrorLogger ()
installApp opts cfg = do
  let apppkgdir = appPackageDir cfg
      copname   = coPackage opts
      copkgdir  = apppkgdir </> coPackage opts
  curdir <- liftIOEL $ getCurrentDirectory
  liftIOEL $ removeDirectoryComplete copkgdir
  logDebug $ "Change into directory " ++ apppkgdir
  inDirectoryEL apppkgdir $ do
    checkoutCmd opts cfg
    logDebug $ "Change into directory " ++ copkgdir
    liftIOEL $ setCurrentDirectory copkgdir
    pkg <- loadPackageSpec "."
    if null (executableSpec pkg)
      then do
        liftIOEL $ setCurrentDirectory curdir
        liftIOEL $ removeDirectoryComplete copkgdir
        fail $ "Package '" ++ name pkg ++
               "' has no executable, nothing installed.\n" ++
               "Hint: use 'cypm add " ++ copname ++
               "' to add new dependency and install it."
      else installCmd (InstallOptions Nothing Nothing False True False) cfg

--- Checks the compiler compatibility.
checkCompiler :: Config -> Package -> ErrorLogger ()
checkCompiler cfg pkg =
  unless (isCompatibleToCompiler cfg pkg) $ error $
    "Current compiler '" ++ showCompilerVersion cfg ++
    "' incompatible to package specification!"

--- Installs the executable specified in the package in the
--- bin directory of CPM (compare .cpmrc).
installExecutable :: Config -> Package -> Maybe String -> ErrorLogger ()
installExecutable cfg pkg mbexec = do
  checkCompiler cfg pkg
  mapM_ (\ (PackageExecutable name mainmod eopts) -> do
           lvl <- getLogLevel
           path <- liftIOEL $ getEnv "PATH"
           logInfo $ "Compiling main module '" ++ mainmod ++
                     "' to generate '" ++ name ++ "'..."
           let (cmpname,_,_,_) = compilerVersion cfg
               cmd = unwords $
                       [":set", if levelGte Debug lvl then "v1" else "v0"
                       , maybe "" id (lookup cmpname eopts)
                       , ":load", mainmod, ":save", ":quit"]
               bindir     = binInstallDir cfg
               binexec    = bindir </> name
           curryCmd (ExecOptions cmd) cfg
           logInfo $ "Installing executable '" ++ name ++ "' into '" ++
                         bindir ++ "'"
           -- renaming might not work across file systems, hence we move:
           showExecCmd (unwords ["mv", mainmod, binexec])
           checkPath path bindir
        )
        (filter installExec (executableSpec pkg))
 where
  installExec (PackageExecutable name _ _) = maybe True (==name) mbexec

  checkPath path bindir =
    if bindir `elem` splitSearchPath path
      then return ()
      else logInfo $
             "It is recommended to add '" ++bindir++ "' to your path!"


uninstallCmd :: UninstallOptions -> Config -> ErrorLogger ()
uninstallCmd (UninstallOptions (Just pkgname) (Just ver)) cfg =
  uninstallPackage cfg pkgname ver
--- uninstalls an application (i.e., binary) provided by a package:
uninstallCmd (UninstallOptions (Just pkgname) Nothing) cfg = do
  let copkgdir  = appPackageDir cfg </> pkgname
  codirexists <- liftIOEL $ doesDirectoryExist copkgdir
  if codirexists
    then do
      pkg <- loadPackageSpec copkgdir
      uninstallPackageExecutable cfg pkg
      liftIOEL $ removeDirectoryComplete copkgdir
      logInfo ("Package '" ++ pkgname ++
                   "' uninstalled from application package cache.")
    else fail $
           "Cannot find executable installed for package '" ++ pkgname ++ "'."
uninstallCmd (UninstallOptions Nothing (Just _)) _ =
  logError "Please provide a package and version number!"
uninstallCmd (UninstallOptions Nothing Nothing) cfg = do
  pkgdir <- getLocalPackageSpec cfg "."
  pkg    <- loadPackageSpec pkgdir
  uninstallPackageExecutable cfg pkg

uninstallPackageExecutable :: Config -> Package -> ErrorLogger ()
uninstallPackageExecutable cfg pkg =
  mapM_ (\ (PackageExecutable name _ _) -> do
           let binexec = binInstallDir cfg </> name
           exf <- liftIOEL $ doesFileExist binexec
           if exf
             then do liftIOEL $ removeFile binexec
                     logInfo $ "Executable '" ++ binexec ++ "' removed"
             else logInfo $ "Executable '" ++ binexec ++ "' not installed")
        (executableSpec pkg)

--- Lists all (compiler-compatible if `lall` is false) packages
--- in the given repository.
listCmd :: ListOptions -> Config -> ErrorLogger ()
listCmd (ListOptions lv currysystem csv cat) cfg = do
  repo <- if cat then getRepositoryWithNameVersionCategory cfg
                 else if currysystem
                        then getRepositoryWithNameVersionSynopsisDeps cfg
                        else getRepositoryWithNameVersionSynopsis cfg
  let listresult = if cat then renderCats (catgroups repo)
                          else renderPkgs (allpkgs repo)
  putStrELM listresult
 where
  -- all packages (and versions if `lv`)
  allpkgs repo = concatMap filterPkgVersions
                   (sortBy (\ps1 ps2 -> name (head ps1) <= name (head ps2))
                           (listPackages repo))
   where
    filterPkgVersions pvs =
      if lv then pvs
            else if currysystem
                   then take 1 (filter (isCompatibleToCompiler cfg) pvs)
                   else take 1 pvs

  -- all categories together with their package names:
  catgroups repo =
    let pkgid p = name p -- ++ '-' : showVersionIfCompatible cfg p
        newpkgs = map (filterCompatPkgs cfg) (listPackages repo)
        catpkgs = concatMap (\p -> map (\c -> (c, pkgid p)) (category p))
                            newpkgs
        nocatps = map pkgid (filter (null . category) newpkgs)
    in map (\cg -> (fst (head cg), map snd cg))
           (groupBy (\ (c1,_) (c2,_) -> c1==c2) (nub $ sortBy (<=) catpkgs)) ++
       if null nocatps then []
                       else [("???", nub $ sortBy (<=) nocatps)]

  renderPkgs pkgs =
    let (colsizes,rows) = packageVersionAsTable cfg pkgs True
    in renderTable colsizes rows

  renderCats catgrps =
    let namelen = foldl max 8 $ map (length . fst) catgrps
        header = [ ["Category", "Packages"]
                 , ["--------", "--------"]]
        rows   = header ++ map (\ (c,ns) -> [c, unwords ns]) catgrps
    in renderTable [namelen + 2, 78 - namelen] rows

  renderTable colsizes rows =
    if csv then showCSV (head rows : drop 2 rows)
           else unlines [render (table rows colsizes), cpmInfo, useUpdateHelp]

--- Returns the first package of a list of packages compatible to the
--- current compiler (according to the given configuration).
--- If there is no compatible package, returns the first one.
filterCompatPkgs :: Config -> [Package] -> Package
filterCompatPkgs cfg pkgs =
  let comppkgs = filter (isCompatibleToCompiler cfg) pkgs
  in if null comppkgs then head pkgs else head comppkgs

-- Format a list of packages by showing their names, synopsis, and versions
-- as table rows. Returns also the column sizes.
packageVersionAsTable :: Config -> [Package] -> Bool -> ([Int],[[String]])
packageVersionAsTable cfg pkgs withversion =
  (colsizes, if withversion then rows else map (take 2) rows)
 where
  namelen = foldl max 4 $ map (length . name) pkgs
  colsizes = if withversion then [namelen + 2, 68 - namelen, 10]
                            else [namelen + 2, 78 - namelen]
  header  = [ ["Name", "Synopsis", "Version"]
            , ["----", "--------", "-------"]]
  formatPkg p = [name p, synopsis p, showVersionIfCompatible cfg p]
  rows    = header ++ map formatPkg pkgs

--- Shows the version of a package if it is compatible with the
--- current compiler, otherwise shows the version in brackets.
showVersionIfCompatible :: Config -> Package -> String
showVersionIfCompatible cfg p =
  let s = showVersion (version p)
  in if isCompatibleToCompiler cfg p then s else '(' : s ++ ")"

cpmInfo :: String
cpmInfo = "Use 'cypm info PACKAGE' for more information about a package."


--- Search in all (compiler-compatible) packages in the given repository.
searchCmd :: SearchOptions -> Config -> ErrorLogger ()
searchCmd (SearchOptions q smod sexec) cfg = do
  let searchaction = if smod then searchExportedModules
                             else if sexec then searchExecutable
                                           else searchNameSynopsisModules
  allpkgs <- searchaction cfg q
  let results = sortBy (\p1 p2 -> name p1 <= name p2)
                       (map (filterCompatPkgs cfg)
                            (map (sortBy (\a b -> version a `vgt` version b))
                                 (groupBy (\a b -> name a == name b)
                                 allpkgs)))
      (colsizes,rows) = packageVersionAsTable cfg results False
  putStrELM $ unlines $
    if null results
      then [ "No packages found for '" ++ q ++ "'", useUpdateHelp ]
      else [ render (table rows colsizes), cpmInfo, useUpdateHelp ]


--- `upgrade` command.
upgradeCmd :: UpgradeOptions -> Config -> ErrorLogger ()
upgradeCmd (UpgradeOptions Nothing) cfg = do
  specDir <- getLocalPackageSpec cfg "."
  cleanCurryPathCache specDir
  logInfo "Upgrading all packages"
  upgradeAllPackages cfg specDir
upgradeCmd (UpgradeOptions (Just pkg)) cfg = do
  specDir <- getLocalPackageSpec cfg "."
  logInfo $ "Upgrade " ++ pkg
  upgradeSinglePackage cfg specDir pkg


--- `link` command.
linkCmd :: LinkOptions -> Config -> ErrorLogger ()
linkCmd (LinkOptions src) cfg = do
  specDir <- getLocalPackageSpec cfg "."
  cleanCurryPathCache specDir
  logInfo $ "Linking '" ++ src ++ "' into local package cache..."
  linkToLocalCache cfg src specDir

--- `add` command:
--- Option `--package`: copy the given package to the repository index
--- and package installation directory so that it is available as
--- any other package.
--- Option `--dependency`: add the package name as a dependency to the
--- current package
--- No option: like `--package` followed by `install` command
addCmd :: AddOptions -> Config -> ErrorLogger ()
addCmd (AddOptions addpkg adddep pkg force) config
  | addpkg    = addPackageToRepository config pkg force True
  | adddep    = addDependencyCmd pkg force config
  | otherwise = do addDependencyCmd pkg force config
                   installCmd (installOpts defaultOptions) config

useForce :: String
useForce = "Use option '-f' or '--force' to overwrite it."

--- `add --dependency` command: add the given package as a new
--- dependency to the current package.
addDependencyCmd :: String -> Bool -> Config -> ErrorLogger ()
addDependencyCmd pkgname force config = do
  allpkgs <- getAllPackageVersions config pkgname False
  case allpkgs of
    [] -> packageNotFoundFailure pkgname
    ps -> case filter (isCompatibleToCompiler config) ps of
            []    -> compatPackageNotFoundFailure config pkgname useUpdateHelp
            (p:_) -> do pkgdir <- getLocalPackageSpec config "."
                        addDepToLocalPackage (version p) pkgdir
 where
  addDepToLocalPackage vers pkgdir = do
    pkgSpec <- loadPackageSpec pkgdir
    let depexists = pkgname `elem` dependencyNames pkgSpec
        newdeps   = addDep [[VGte vers, VLt (nextMajor vers)]]
                           (dependencies pkgSpec)
        newpkg    = pkgSpec { dependencies = newdeps }
    if force || not depexists
      then do liftIOEL $ writePackageSpec newpkg (pkgdir </> packageSpecFile)
              logInfo $ "Dependency '" ++ pkgname ++ " >= " ++
                            showVersion vers ++
                            "' added to package '" ++ pkgdir ++ "'"
      else logCritical $ "Dependency '" ++ pkgname ++
                             "' already exists!\n" ++ useForce

  addDep vcs [] = [Dependency pkgname vcs]
  addDep vcs (Dependency pn pvcs : deps) =
    if pn == pkgname then Dependency pn vcs : deps
                     else Dependency pn pvcs : addDep vcs deps

------------------------------------------------------------------------------
--- `doc` command: run `curry doc` on the modules provided as an argument
--- or, if they are not given, on exported modules (if specified in the
--- package), on the main executable (if specified in the package),
--- or on all source modules of the package.
docCmd :: DocOptions -> Config -> ErrorLogger ()
docCmd opts cfg = do
  specDir <- getLocalPackageSpec cfg "."
  pkg     <- loadPackageSpec specDir
  let docdir = maybe "cdoc" id (docDir opts) </> packageId pkg
  absdocdir <- liftIOEL $ getRealPath docdir
  liftIOEL $ createDirectoryIfMissing True absdocdir
  when (docReadme   opts) $ genPackageREADME pkg specDir absdocdir
  when (docManual   opts) $ genPackageManual pkg specDir absdocdir
  when (docPrograms opts) $ genDocForPrograms opts cfg absdocdir specDir pkg

--- Translate package README file to HTML, if possible (i.e., some README
--- file and `pandoc` exists). Two README files are produced:
--- `README.html` (standalone document) and `README_I.html` (document
--- fragment without header and footer).
genPackageREADME :: Package -> String -> String -> ErrorLogger ()
genPackageREADME _ specDir outputdir = do
  rmfiles  <- getReadmeFiles
  ispandoc <- liftIOEL $ fileInPath "pandoc"
  if null rmfiles || not ispandoc
    then do
      logInfo $ "'README.html' not generated: " ++
                    if ispandoc then "no README file found"
                                else "executable 'pandoc' not found"
    else do
      let readmefile = head rmfiles
          formatcmd1 = formatCmd1 readmefile
          formatcmd2 = formatCmd2 readmefile
      logDebug $ "Executing command: " ++ formatcmd1
      rc1 <- inDirectoryEL specDir $ showExecCmd formatcmd1
      logDebug $ "Executing command: " ++ formatcmd2
      rc2 <- inDirectoryEL specDir $ showExecCmd formatcmd2
      if rc1 == 0 && rc2 == 0
        then do
          -- make them readable:
          showExecCmd $
            unwords ["chmod -f 644 ", quote outfile1, quote outfile2]
          logInfo $
            "'" ++ readmefile ++ "' translated to '" ++ outfile1 ++ "'."
        else fail $ "Error during execution of commands:\n" ++
                       formatcmd1 ++ "\n" ++ formatcmd2
 where
  outfile1 = outputdir </> "README.html"
  outfile2 = outputdir </> "README_I.html"

  getReadmeFiles = do
    entries <- liftIOEL $ getDirectoryContents specDir
    return $ filter ("README" `isPrefixOf`) entries

  pandocCmd = "pandoc -f gfm -t html "
  formatCmd1 readme = pandocCmd ++ "-s -o " ++ outfile1 ++ " " ++ readme
  formatCmd2 readme = pandocCmd ++ "-o " ++ outfile2 ++ " " ++ readme

--- Generate manual according to  documentation specification of package.
genPackageManual :: Package -> String -> String -> ErrorLogger ()
genPackageManual pkg specDir outputdir = case documentation pkg of
    Nothing -> return ()
    Just (PackageDocumentation docdir docmain doccmd) -> do
      let formatcmd = replaceSubString "OUTDIR" outputdir $
                        if null doccmd then formatCmd docmain
                                       else doccmd
      if null formatcmd
        then logInfo $ "Cannot format documentation file '" ++
                           docmain ++ "' (unknown kind)"
        else do
          logDebug $ "Executing command: " ++ formatcmd
          rc <- inDirectoryEL (specDir </> docdir) $ showExecCmd formatcmd
          if rc == 0
            then do
              let outfile = outputdir </> replaceExtension docmain ".pdf"
               -- make it readable:
              showExecCmd $ "chmod -f 644 " ++ quote outfile
              logInfo $
                "Package documentation written to '" ++ outfile ++ "'."
            else fail $ "Error during execution of command:\n" ++ formatcmd
 where
  formatCmd docmain
    | ".tex" `isSuffixOf` docmain
    = let formatcmd = "pdflatex -output-directory=\"OUTDIR\" " ++ docmain
      in formatcmd ++ " && " ++ formatcmd
    | ".md" `isSuffixOf` docmain
    = "pandoc -f gfm " ++ docmain ++
      " -o \"OUTDIR" </> replaceExtension docmain ".pdf" ++ "\""
    | otherwise = ""

--- Replace every occurrence of the first argument by the second argument
--- in a string (third argument).
replaceSubString :: String -> String -> String -> String
replaceSubString sub newsub s = replString s
 where
  sublen = length sub

  replString [] = []
  replString ccs@(c:cs) =
    if take sublen ccs == sub
      then newsub ++ replString (drop sublen ccs)
      else c : replString cs

--- Generate program documentation:
--- run `curry-doc` on the modules provided as an argument
--- or, if they are not given, on exported modules (if specified in the
--- package), on the main executable (if specified in the package),
--- or on all source modules of the package.
genDocForPrograms :: DocOptions -> Config -> String -> String -> Package
                  -> ErrorLogger ()
genDocForPrograms opts cfg docdir specDir pkg = do
  abspkgdir <- liftIOEL $ getRealPath specDir
  checkCompiler cfg pkg
  let exports  = exportedModules pkg
      mainmods = map (\ (PackageExecutable _ emain _) -> emain)
                     (executableSpec pkg)
  (docmods,apidoc) <-
     maybe (if null exports
              then if null mainmods
                     then (do ms <- getSourceModulesOfPkg specDir pkg
                              return (ms,True))
                     else return (mainmods,False)
              else return (exports,True))
           (\ms -> return (ms,True))
           (docModules opts)
  if null docmods
    then logInfo "No modules to be documented!"
    else do
      currypath <- getCurryLoadPath cfg specDir
      let pkgurls = path2packages abspkgdir currypath
      if apidoc
        then do
          mapM_ (docModule currypath pkgurls) docmods
          runDocCmd currypath pkgurls
            (["--title", apititle, "--onlyindexhtml", docdir] ++ docmods)
          logInfo ("Documentation generated in '"++docdir++"'")
        else runDocCmd currypath pkgurls [docdir, head docmods]
 where
  apititle = "\"Package " ++ name pkg ++ "\""

  docModule currypath uses mod =
    runDocCmd currypath uses ["--noindexhtml", docdir, mod]

  runDocCmd currypath uses docparams = do
    currydoc <- getCurryDoc cfg
    let useopts = if docGenImports opts
                    then []
                    else map (\ (d,u) -> "--use "++d++"@"++u) uses
        cmd = unwords (currydoc : useopts ++ docparams)
    logInfo $ "Running CurryDoc: " ++ cmd
    execWithCurryPath (ExecOptions cmd) cfg currypath

  -- translates a path into a list of package paths and their doc URLs:
  path2packages absdir path =
    let dirs = splitSearchPath path
        importPkgDir = absdir </> ".cpm" </> "packages" ++ [pathSeparator]
        isImportedPackage d = importPkgDir `isPrefixOf` d
        impPkg2URL p =
          let (d,_) = break isPathSeparator (drop (length importPkgDir) p)
          in docPackageURL opts ++ "/" ++ d
    in map (\ip -> (ip,impPkg2URL ip)) (filter isImportedPackage dirs) ++
       if name pkg == "base"
         then [] -- in order to generate base package documentation
         else [(installDir </> "lib",
                docPackageURL opts ++ "/base-" ++ compilerBaseVersion cfg)]

------------------------------------------------------------------------------
--- `test` command: run `curry-check` on the modules provided as an argument
--- or, if they are not provided, on the exported (if specified)
--- or all source modules of the package.
testCmd :: TestOptions -> Config -> ErrorLogger ()
testCmd opts cfg = do
  specDir <- getLocalPackageSpec cfg "."
  pkg     <- loadPackageSpec specDir
  checkCompiler cfg pkg
  aspecDir  <- liftIOEL $ getRealPath specDir
  mainprogs <- getSourceModulesOfPkg aspecDir pkg
  mbcc      <- if testCompile opts then return Nothing
                                   else getCurryCheck cfg
  when (isNothing mbcc) $ logInfo "No tests, just compiling..."
  let pkg'  = maybe (pkg { testSuite = Nothing }) (const pkg) mbcc
      tests = testSuites pkg' mainprogs
  stats <- if null tests
             then do logInfo "No modules to be tested!"
                     return []
             else mapM (execTest mbcc aspecDir) tests
  unless (null (testFile opts)) $ liftIOEL $
    combineCSVStatsOfPkg (packageId pkg) (concat stats) (testFile opts)
 where
  execTest Nothing apkgdir (PackageTest dir mods _ _) = do
    logInfo $ "Compiling modules:" ++ concatMap (' ':) mods
    let cmpcmd = unwords $ [curryExec cfg] ++
                           concatMap (\m -> [":load", m]) mods ++ [":quit"]
    inDirectoryEL (apkgdir </> dir) $ do
      execWithPkgDir (ExecOptions $ cmpcmd) cfg apkgdir
      return []

  execTest (Just currycheck) apkgdir (PackageTest dir mods pccopts script) = do
    pid <- liftIOEL getPID
    let csvfile  = "TESTRESULT" ++ show pid ++ ".csv"
        statopts = if null (testFile opts) then [] else ["statfile=" ++ csvfile]
        tccopts  = unwords (map ("--" ++) (testCheckOpts opts ++ statopts) ++
                            (if testSafe opts then ["--noiotest"] else []))
        ccopts   = if null tccopts
                     then pccopts
                     else if null pccopts then tccopts
                                          else tccopts ++ ' ' : pccopts
        scriptcmd = "CURRYBIN=" ++ curryExec cfg ++ " && export CURRYBIN && " ++
                    "." </> script ++ if null pccopts then "" else ' ' : pccopts
        checkcmd  = currycheck ++ if null ccopts then "" else ' ' : ccopts
    unless (null mods) $ putStrLnELM $
      "Running CurryCheck (" ++ checkcmd ++ ")\n" ++
      "(in directory '" ++ dir ++ "', showing raw output) on modules:\n" ++
      unwords mods ++ "\n"
    unless (null script) $ putStrLnELM $
      "Executing test script with command:\n" ++ scriptcmd ++ "\n" ++
      "(in directory '" ++ dir ++ "', showing raw output):\n"
    let currysubdir = apkgdir </> addCurrySubdir dir
        testcmd = if not (null mods)
                    then unwords (checkcmd : mods)
                    else scriptcmd
    logDebug $ "Removing directory: " ++ currysubdir
    showExecCmd (unwords ["rm", "-rf", currysubdir])
    inDirectoryEL (apkgdir </> dir) $ do
      execWithPkgDir (ExecOptions testcmd) cfg apkgdir
      if null (testFile opts) || null mods
        then return []
        else do s <- liftIOEL $ readCompleteFile csvfile
                liftIOEL $ removeFile csvfile
                return [readCSV s]

  testSuites spec mainprogs = case testModules opts of
    Nothing -> maybe (let exports = exportedModules spec
                      in if null exports
                           then if null mainprogs
                                  then []
                                  else [PackageTest "src" mainprogs "" ""]
                           else [PackageTest "src" exports "" ""])
                     (filter allowedTest)
                     (testSuite spec)
    Just ms -> [PackageTest "src" ms "" ""]
   where
    allowedTest (PackageTest _ _ _ scrpt) = not (testSafe opts) || null scrpt

-- Combine all CSV statistics (produced by CurryCheck for a package)
-- into one file in CSV format by accumulating all numbers and modules.
combineCSVStatsOfPkg :: String -> [[[String]]] -> String -> IO ()
combineCSVStatsOfPkg pkgid csvs outfile = do
  ltime <- fmap toUTCTime getClockTime
  let results = foldr addStats ([], take 6 (repeat 0), "") (map fromCSV csvs)
  writeCSVFile outfile (showStats (calendarTimeToString ltime) results)
 where
  fromCSV rows =
    let [rc,total,unit,prop,eqv,io,mods] = rows !! 1
    in (rows !! 0, map (\s -> read s :: Int) [rc,total,unit,prop,eqv,io], mods)

  showStats ct (header,nums,mods) =
    ["Package" : "Check time" : header, pkgid : ct : map show nums ++ [mods]]

  addStats (header,nums1,mods1) (_,nums2,mods2) =
    (header, map (uncurry (+)) (zip nums1 nums2), mods1 ++ " " ++ mods2)

------------------------------------------------------------------------------
-- Returns the names of all Curry modules in the source dirs of an
-- installed package.
-- The first argument is the installation directory of the package.
getSourceModulesOfPkg :: String -> Package -> ErrorLogger [String]
getSourceModulesOfPkg specdir pkg =
  mapM (liftIOEL . curryModulesInDirectory)
       (map (specdir </>) (sourceDirsOf pkg))
    >>= return . concat

------------------------------------------------------------------------------
-- `diff` command:
diffCmd :: DiffOptions -> Config -> ErrorLogger ()
diffCmd opts cfg = do
  specDir   <- getLocalPackageSpec cfg "."
  localSpec <- loadPackageSpec specDir
  checkCompiler cfg localSpec
  let localname  = name localSpec
      localv     = version localSpec
      showlocalv = showVersion localv
  repo <- getRepoForPackageSpec cfg localSpec
  diffv <- getDiffVersion repo localname
  if diffv == localv
    then fail $ "Cannot diff identical package versions " ++ showlocalv
    else do
      putStrLnELM $ "Comparing local version " ++ showlocalv ++
                    " and repository version " ++ showVersion diffv ++ ":\n"
      installIfNecessary repo localname diffv
      putStrLnELM ""
      gc <- readGlobalCache cfg repo
      diffAPIIfEnabled      repo gc specDir localSpec diffv
      diffBehaviorIfEnabled repo gc specDir localSpec diffv
      liftIOEL $ cleanTempDir
 where
  getDiffVersion repo localname = case diffVersion opts of
    Nothing -> case findLatestVersion cfg repo localname False of
      Nothing -> fail $
        "No other version of local package '" ++ localname ++
        "' compatible to '" ++ showCompilerVersion cfg ++
        "' found in package repository."
      Just p  -> return (version p)
    Just v  -> return v

  installIfNecessary repo pkgname ver =
    case findVersion repo pkgname ver of
      Nothing -> packageNotFoundFailure $ pkgname ++ "-" ++ showVersion ver
      Just  p -> acquireAndInstallPackageWithDependencies cfg repo p

  diffAPIIfEnabled repo gc specDir localSpec diffversion =
    when (diffAPI opts) $ do
      putStrLnELM "Running API diff...\n"
      diffResults <- APIDiff.compareModulesFromPackageAndDir cfg repo gc specDir
                            (name localSpec) diffversion (diffModules opts)
      let diffOut = APIDiff.showDifferences (map snd diffResults)
                                            (version localSpec) diffversion
      unless (null diffOut) (putStrLnELM diffOut >> putStrLnELM "")

  diffBehaviorIfEnabled repo gc specDir localSpec diffversion =
    when (diffBehavior opts) $ do
      putStrLnELM "Preparing behavior diff...\n"
      i <- BDiff.preparePackageAndDir cfg repo gc specDir (name localSpec)
                                                          diffversion
      BDiff.diffBehavior cfg repo gc i (diffGroundEqu opts)
                         (diffUseAna opts) (diffModules opts)

-- Implementation of the `curry` command.
curryCmd :: ExecOptions -> Config -> ErrorLogger ()
curryCmd o cfg = do
  pkgdir <- getLocalPackageSpec cfg "."
  pkg    <- loadPackageSpec pkgdir
  checkCompiler cfg pkg
  execWithPkgDir
    (ExecOptions $ unwords [curryExec cfg, "--nocypm", exeCommand o])
    cfg pkgdir

-- Implementation of the `exec` command.
execCmd :: ExecOptions -> Config -> ErrorLogger ()
execCmd o cfg = do
  pkgdir <- getLocalPackageSpec cfg "."
  execWithPkgDir o cfg pkgdir

-- Execute a command with the load path set to the package stored
-- in a directory (third argument).
execWithPkgDir :: ExecOptions -> Config -> String -> ErrorLogger ()
execWithPkgDir o cfg specDir = do
  cp <- getCurryLoadPath cfg specDir
  execWithCurryPath o cfg cp

execWithCurryPath :: ExecOptions -> Config -> String -> ErrorLogger ()
execWithCurryPath o _ currypath = do
  logDebug $ "Setting CURRYPATH to " ++ currypath
  liftIOEL $ setEnv "CURRYPATH" currypath
  ecode <- showExecCmd (exeCommand o)
  liftIOEL $ unsetEnv "CURRYPATH"
  liftIOEL $ unless (ecode==0) (exitWith ecode)

-- Compute the load path of the package as a string where
-- the directory of the `base` package is removed.
computePackageLoadPath :: Config -> String -> ErrorLogger String
computePackageLoadPath cfg pkgdir = do
  logDebug "Computing load path for package..."
  pkg <- loadPackageSpec pkgdir
  allpkgs <- resolveAndCopyDependenciesForPackage cfg pkgdir pkg
  abs <- liftIOEL $ getRealPath pkgdir
  let srcdirs = map (abs </>) (sourceDirsOf pkg)
      -- remove 'base' package since it is in the compiler libraries:
      pkgs = filter (\p -> name p /= "base") allpkgs
      currypath = joinSearchPath (srcdirs ++ dependencyPathsSeparate pkgs abs)
  liftIOEL $ saveCurryPathToCache cfg pkgdir currypath
  return currypath

------------------------------------------------------------------------------
--- Implementation of the `init` command: initialize a new package inside
--- the current directory.
initCmd :: Config -> ErrorLogger ()
initCmd cfg = do
  pname <- liftIOEL $ (getCurrentDirectory >>= return . takeFileName)
  pkgexists <- liftIOEL $ doesFileExist packageSpecFile
  if pkgexists
    then do
      logError $
        "There is already a package specification file '" ++ packageSpecFile ++
        "'.\nI cannot initialize a new package!"
      liftIOEL $ exitWith 1
    else liftIOEL $ initPackage cfg pname $
      "A new package '" ++ pname ++
      "' has been created in the current directory.\n\nNow "

initPackage :: Config -> String -> String -> IO ()
initPackage cfg pname outheader = do
  let emptyAuthor   = "YOUR NAME <YOUR EMAIL ADDRESS>"
      emptySynopsis = "PLEASE PROVIDE A ONE-LINE SUMMARY ABOUT THE PACKAGE"
      basedeps      = maybe []
                            (\v -> [Dependency "base" [[VMajCompatible v]]])
                            (readVersion (compilerBaseVersion cfg))
  let pkgSpec = emptyPackage { name            = pname
                             , version         = initialVersion
                             , author          = [emptyAuthor]
                             , synopsis        = emptySynopsis
                             , category        = ["Programming"]
                             , dependencies    = basedeps
                             , exportedModules = []
                             , license         = Just "BSD-3-Clause"
                             , licenseFile     = Just "LICENSE"
                             }
  writePackageSpec pkgSpec packageSpecFile
  let licenseFile  = "LICENSE"
      licenseTFile = packagePath </> "templates" </> licenseFile
  whenFileExists licenseTFile $
    ifFileExists licenseFile (return ()) (copyFile licenseTFile licenseFile)
  createDirectoryIfMissing False "src"
  let cmain     = "Main.curry"
      mainTFile = packagePath </> "templates" </> cmain
      mainSFile = "src" </> cmain
  whenFileExists mainTFile $
    ifFileExists mainSFile (return ()) (copyFile mainTFile mainSFile)
  writeFileIfNotExists "README.md" readme
  writeFileIfNotExists ".gitignore" gitignore
  putStr $ outheader ++ unlines todo
 where
  readme = unlines [pname, take (length pname) (repeat '=')]
  gitignore = unlines ["*~", ".cpm", ".curry"]

  todo =
    [ "edit the file '" ++ packageSpecFile ++ "':"
    , "- enter correct values for the fields 'author', 'synopsis', 'category'"
    , "- add dependencies in the field 'dependencies'"
    , "- add further fields (e.g., 'description')"
    , "- review field 'license' (and adapt file 'LICENSE')"
    , ""
    , "Then run 'cypm install' to install all dependencies and"
    , "put your program code in directory 'src'"
    , "(where you find a template file 'Main.curry')"
    , ""
    , "Run the main program with:"
    , "> cypm curry :load Main :eval main :quit"
    ]

--- Implementation of the `new` command: create a new package.
newCmd :: NewOptions -> Config -> ErrorLogger ()
newCmd (NewOptions pname) cfg = do
  exists <- liftIOEL $ doesDirectoryExist pname
  if exists
    then do
      logError $ "There is already a directory with the new project name.\n"
                  ++ "I cannot create new project!"
      liftIOEL $ exitWith 1
    else liftIOEL $ do
      createDirectory pname
      setCurrentDirectory pname
      initPackage cfg pname $
        "A new package has been created in the directory '" ++ pname ++
        "'.\n\nGo into this directory and "

------------------------------------------------------------------------------
--- Implementation of the `clean` command: clean the current package.
cleanCmd :: CleanOptions -> Config -> ErrorLogger ()
cleanCmd opts config = do
  cleanPackage config Info
  when (cleanDeps opts) cleanDepsInLocalPackage
 where
  cleanDepsInLocalPackage = do
    pkgdir  <- getLocalPackageSpec config "."
    pkgSpec <- loadPackageSpec pkgdir
    let pkgfile     = pkgdir </> packageSpecFile
        pkgbakfile  = pkgfile ++ ".BAK"
        homepkgspec = homePackageDir config </> packageSpecFile
        newpkg      = pkgSpec { dependencies = [] }
    unless (null (dependencies pkgSpec)) $ do
      confirm <- if equalFilePath pkgfile homepkgspec
                   then return True -- do not ask to clean home package
                   else fmap (/="no") $ liftIOEL $ askYesNo $
                          "Really remove dependencies in '" ++ pkgfile ++
                          "'? (Yes|no) "
      if confirm
        then do
          liftIOEL $ do renameFile pkgfile pkgbakfile
                        writePackageSpec newpkg pkgfile
          logInfo $ "Dependencies deleted in package '" ++ pkgdir ++ "'"
          logInfo $ "(old package file saved into '" ++ pkgbakfile ++ "')"
        else logInfo "Dependencies not removed"

------------------------------------------------------------------------------
--- Uploads the current package to the package server.
--- If the package has a GIT repository with tag `$version`
--- and the option `--notagging` is not given, the package version
--- is set as a tag (in the local GIT repository) and then pushed
--- to the repostory.
--- Then, the current package is tested (as with `cypm test`).
--- If the test fails, the package is not uploaded.
--- If the test succeeds, an existing locally installed package
--- is removed, the package is added to the local copy of the repository,
--- and the `package.json` is uploaded via the script specified
--- in `uploadURL`.
uploadCmd :: UploadOptions -> Config -> ErrorLogger ()
uploadCmd opts cfg = do
  specDir <- getLocalPackageSpec cfg "."
  lpkg    <- loadPackageSpec  specDir
  let pkgrepodir = repositoryDir cfg </>
                   name lpkg </> showVersion (version lpkg)
  exrepodir <- liftIOEL $ doesDirectoryExist pkgrepodir
  if exrepodir && not (forceUpdate opts)
    then fail "Package version already exists in repository!"
    else return ()
  inDirectoryEL specDir $ setTagInGitIfNecessary opts lpkg
  instdir <- liftIOEL tempDir
  liftIOEL $ recreateDirectory instdir
  installPkg lpkg instdir
  let pkgid = packageId lpkg
  pkg <- loadPackageSpec (instdir </> pkgid)
  -- Test package if CurryCheck is installed:
  mbccfile <- liftIOEL $ getFileInPath "curry-check"
  ecode <- maybe (return 0) (\_ -> testPackage pkgid instdir) mbccfile
  if ecode > 0
    then do liftIOEL cleanTempDir
            logCritical "ERROR in package, package not uploaded!"
    else do
      logInfo $ take 70 (repeat '=')
      logInfo $ "Uploading package '" ++ pkgid ++
                "' to global Masala repository..."
      -- remove package possibly existing in global package cache:
      liftIOEL $ removeDirectoryComplete (installedPackageDir cfg pkg)
      uploadPackageSpec2Masala opts (instdir </> pkgid </> packageSpecFile)
      addPackageToRepo pkgrepodir (instdir </> pkgid) pkg
      liftIOEL $ cleanTempDir
      logInfo $ "Package '" ++ pkgid ++ "' uploaded"
 where
  -- add package to local copy of repository:
  addPackageToRepo pkgrepodir pkgdir pkg = do
    exrepodir <- liftIOEL $ doesDirectoryExist pkgrepodir
    logDebug $ "Create directory: " ++ pkgrepodir
    liftIOEL $ do
      createDirectoryIfMissing True pkgrepodir
      copyFile (pkgdir </> packageSpecFile) (pkgrepodir </> packageSpecFile)
    if exrepodir then updatePackageInRepositoryCache cfg pkg
                 else addPackageToRepositoryCache    cfg pkg

  installPkg pkg instdir = case source pkg of
    Nothing            -> fail $ "No source specified for package"
    Just (Git url rev) -> installPackageSourceTo pkg (Git url rev) instdir
    _                  -> fail $ "No git source with version tag"

  testPackage pkgid instdir = do
    curdir <- inDirectoryEL instdir $ liftIOEL $ getCurrentDirectory
    let bindir = curdir </> "pkgbin"
    liftIOEL $ recreateDirectory bindir
    let cmd = unwords
                [ -- install possible binaries in bindir:
                  "cypm", "-d bin_install_path="++bindir, "install", "&&"
                , "export PATH=" ++ bindir ++ ":$PATH", "&&"
                , "cypm", "test", "&&"
                , "cypm", "-d bin_install_path="++bindir, "uninstall"
                ]
    putStrLnELM $ "Testing package: '" ++ pkgid ++ "' with command:\n" ++ cmd
    inDirectoryEL (instdir </> pkgid) $ showExecCmd cmd

--- Set the package version as a tag in the local GIT repository and push it,
--- if the package source is a GIT with tag `$version` and the option `setTag`
--- is `True`. If the option `setTag` is `False`, ask the user whether
--- to set the tag.
setTagInGitIfNecessary :: UploadOptions -> Package -> ErrorLogger ()
setTagInGitIfNecessary opts pkg
  | setTag opts && hasGitVersionSource = setTagInGit pkg
  | setTag opts = putStrLnELM $ "Repository not tagged since the source is not"
                    ++ "\nis not a git repository with tag '$version'."
  | not hasGitVersionSource = return ()
  | otherwise -- ask the user for tagging:
  = do answer <- liftIOEL $ askYesNo tagQuestion
       if null answer || answer == "yes" then setTagInGit pkg else return ()
 where
  tagQuestion = "Should I tag the git repository with tag '" ++ tag ++
                "'? (Yes|no) "

  hasGitVersionSource = case source pkg of
    Just (Git _ (Just VersionAsTag)) -> True
    _                                -> False

  tag = 'v' : showVersion (version pkg)

setTagInGit :: Package -> ErrorLogger ()
setTagInGit pkg = do
  let ts = 'v' : showVersion (version pkg)
  logInfo $ "Tagging current git repository with tag '" ++ ts++ "'"
  (_,gittag,_) <- liftIOEL $ evalCmd "git" ["tag","-l",ts] ""
  let deltag = if null gittag then [] else ["git tag -d",ts,"&&"]
      cmd    = unwords $ deltag ++ ["git tag -a",ts,"-m",ts,"&&",
                                    "git push --tags -f"]
  logInfo $ "Execute: " ++ cmd
  ecode <- showExecCmd cmd
  if ecode == 0 then return ()
                else fail $ "ERROR in setting the git tag"

-- Uploads a package specification stored in a file (first argument,
-- like `.../package.json`) to Masala via URL uploading.
uploadPackageSpec2Masala :: UploadOptions -> String -> ErrorLogger ()
uploadPackageSpec2Masala opts pkgspecfname = do
  pkgspec <- liftIOEL $ readFile pkgspecfname
  (login,cryptpass) <- liftIOEL getLoginData
  if null login
    then fail "Package not uploaded to global Masala repository!"
    else do
      (curlcmd,copts) <- getCurlCmdOpts
      let uploadurl = masalaUploadURL login cryptpass (uploadPublish opts)
                                      (forceUpdate opts) pkgspec
          curlopts  = copts ++ [uploadurl]
      logDebug $ unwords (curlcmd : curlopts) ++ "\n" ++ pkgspec
      (rc,out,err) <- liftIOEL $ evalCmd curlcmd curlopts ""
      unless (null out) $ logInfo out
      if rc == 0 && not ("ERROR" `isInfixOf` out)
        then return ()
        else do logInfo err
                logInfo "UPLOAD TO GLOBAL MASALA REPOSITORY FAILED!"
                liftIOEL $ putStrLn
                  "\nYou can try to upload again with new login / password:"
                let newopts = opts { uploadLogin = "", uploadPasswd = ""}
                uploadPackageSpec2Masala newopts pkgspecfname
 where
  getLoginData = do
    login <- if null (uploadLogin opts)
               then do putStr "Your Masala login name (leave empty to stop): "
                       hFlush stdout
                       getLine
               else return $ uploadLogin opts
    pass <- if not (null login) && null (uploadPasswd opts)
              then do putStr $ "Masala password of '" ++ login ++ "': "
                      hFlush stdout
                      system "stty -echo"
                      realpasswd <- getLine
                      system "stty echo"
                      putChar '\n'
                      return realpasswd
              else return $ uploadPasswd opts
    cryptpasswd <- getHash (login ++ pass ++ "3masala25")
    putStrLn ""
    return (login,cryptpasswd)

-- URL of cpm-upload script.
-- The arguments are the login name, the password (encrypted by the method
-- used in Masala), the publish and force flags, and the text of the
-- package specification.
masalaUploadURL :: String -> String -> Bool -> Bool -> String -> String
masalaUploadURL login cryptpass publish force pkgtxt =
  "https://cpm.curry-lang.org/masala/run.cgi?UploadBy/" ++
  intercalate "/"
    (map string2urlencoded [login, cryptpass, show publish, show force, pkgtxt])

------------------------------------------------------------------------------
--- Fail with a "package not found" message.
packageNotFoundFailure :: String -> ErrorLogger _
packageNotFoundFailure pkgname =
  fail $ "Package '" ++ pkgname ++ "' not found in package repository.\n" ++
            useUpdateHelp

--- Fail with a "compatible package not found" message and a comment
compatPackageNotFoundFailure :: Config -> String -> String -> ErrorLogger _
compatPackageNotFoundFailure cfg pkgname helpcmt =
  fail $ "No version of package '" ++ pkgname ++ "' compatible to '" ++
            showCompilerVersion cfg ++ "' found!\n" ++
            helpcmt

---------------------------------------------------------------------------
-- Caching the current CURRYPATH of a package for faster startup.
-- The file `.cpm/CURRYPATH_CACHE` contains the following lines:
-- * The CURRYPATH used to load the package
-- * The compiler name and major/minor/revision version
-- * The version of the base libraries required during package install

--- The name of the cache file in a package directory.
curryPathCacheFile :: String -> String
curryPathCacheFile pkgdir = pkgdir </> ".cpm" </> "CURRYPATH_CACHE"

--- Saves package CURRYPATH in local cache file in the given package dir.
saveCurryPathToCache :: Config -> String -> String -> IO ()
saveCurryPathToCache cfg pkgdir path = do
  let cpmdir = pkgdir </> ".cpm"
  createDirectoryIfMissing False cpmdir
  writeFile (curryPathCacheFile pkgdir)
            (unlines [path, showCompilerVersion cfg, compilerBaseVersion cfg])

--- Gets CURRYPATH of the given package (either from the local cache file
--- in the package dir or compute it). The directory of the `base` package
--- is removed from the path since it is part of the Curry system libraries.
getCurryLoadPath :: Config -> String -> ErrorLogger String
getCurryLoadPath cfg pkgdir = do
  mbp <- loadCurryPathFromCache cfg pkgdir
  maybe (computePackageLoadPath cfg pkgdir) return mbp

--- Writes the full load path in JSON format (i.e., an array of strings)
--- into the file `.vscode/settings.json` (or update an existing file)
--- so that the Curry Language Server has access to all imported modules.
--- The directory of the `base` package is removed from the path
--- and replaced by the libraries of the current Curry system.
setVSCodeImportPath :: Config -> String -> ErrorLogger ()
setVSCodeImportPath cfg pkgdir = do
  mbp <- loadCurryPathFromCache cfg pkgdir
  loadpath <- maybe (computePackageLoadPath cfg pkgdir) return mbp
  let jdirs = JArray (map JString (splitSearchPath loadpath ++ sysLibPath))
  updateVSCodeSettings jdirs
 where
  pathkey = "curry.languageServer.importPaths"

  vssettingsFile = ".vscode" </> "settings.json"

  updateVSCodeSettings jdirs = do
    vssetexists <- liftIOEL $ doesFileExist vssettingsFile
    if vssetexists
      then do
        liftIOEL $ updateFile
          (\s -> ppJSON (maybe (JObject [(pathkey,jdirs)])
                               (updateJObjectKey pathkey jdirs)
                               (parseJSON s)))
          vssettingsFile
        logInfo $ "File '" ++ vssettingsFile ++ "' updated."
      else do
        liftIOEL $ createDirectoryIfMissing True
          (joinPath (init (splitDirectories vssettingsFile)))
        liftIOEL $ writeFile vssettingsFile (ppJSON (JObject [(pathkey,jdirs)]))
        logInfo $ "File '" ++ vssettingsFile ++ "' created."

--- Replaces a key-value pair in a given JSON object. If the given JSON value
--- is not an object, returns a JSON object with this key-value pair.
updateJObjectKey :: String -> JValue -> JValue -> JValue
updateJObjectKey key val jobject = case jobject of
  JObject keyvals -> maybe (JObject $ keyvals ++ [(key,val)])
                           (\i -> JObject (replace (key,val) i keyvals))
                           (findIndex ((==key) . fst) keyvals)
  _               -> JObject [(key,val)]

--- Writes the full load path in JSON format (i.e., an array of strings)
--- into the file `.curry/language-server/paths.json` so that
--- the Curry Language Server has access to all imported modules.
--- The directory of the `base` package is removed from the path
--- and replaced by the libraries of the current Curry system.
setLanguageServerImportPath :: Config -> String -> ErrorLogger ()
setLanguageServerImportPath cfg pkgdir = do
  mbp <- loadCurryPathFromCache cfg pkgdir
  loadpath <- maybe (computePackageLoadPath cfg pkgdir) return mbp
  let jdirs = JArray (map JString (splitSearchPath loadpath ++ sysLibPath))
  liftIOEL $ do
    createDirectoryIfMissing True lspDir
    writeFile lspFile (ppJSON jdirs)
  logInfo $ "File '" ++ lspFile ++ "' written."
 where
  lspDir  = ".curry" </> "language-server"
  lspFile = lspDir </> "paths.json"

--- Restores package CURRYPATH from local cache file in the given package dir,
--- if it is still up-to-date, i.e., it exists and is newer than the package
--- specification.
loadCurryPathFromCache :: Config -> String -> ErrorLogger (Maybe String)
loadCurryPathFromCache cfg pkgdir = do
  let cachefile = curryPathCacheFile pkgdir
  excache <- liftIOEL $ doesFileExist cachefile
  if excache
    then do
      cftime <- liftIOEL $ getModificationTime cachefile
      pftime <- liftIOEL $ getModificationTime (pkgdir </> packageSpecFile)
      if cftime > pftime
        then do cnt <- liftIOEL $ safeReadFile cachefile
                let ls = either (const []) lines cnt
                return $ if consistentCache ls then Just (head ls)
                                               else Nothing
        else return Nothing
    else return Nothing
 where
  consistentCache cls =
    length cls > 2 && cls!!1 == showCompilerVersion cfg
                   && cls!!2 == compilerBaseVersion cfg

--- Cleans the local cache file for CURRYPATH. This might be necessary
--- for upgrade/install/link commands.
cleanCurryPathCache :: String -> ErrorLogger ()
cleanCurryPathCache pkgdir = liftIOEL $ do
  let pathcachefile = curryPathCacheFile pkgdir
  whenFileExists pathcachefile $ removeFile pathcachefile
  return ()

------------------------------------------------------------------------------
