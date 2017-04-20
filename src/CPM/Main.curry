--------------------------------------------------------------------------------
--- This is the main module of the Curry Package Manager.
--------------------------------------------------------------------------------

module CPM.Main where

import Char         ( toLower )
import CSV          ( showCSV )
import Directory    ( doesFileExist, getAbsolutePath, doesDirectoryExist
                    , copyFile, createDirectory, createDirectoryIfMissing
                    , getDirectoryContents, getModificationTime
                    , renameFile, removeFile, setCurrentDirectory )
import Distribution ( stripCurrySuffix, addCurrySubdir )
import Either
import FilePath     ( (</>), splitSearchPath, takeExtension )
import IO           ( hFlush, stdout )
import List         ( groupBy, intercalate, nub, split, splitOn )
import Sort         ( sortBy )
import System       ( getArgs, getEnviron, setEnviron, system, unsetEnviron
                    , exitWith )

import Boxes (table, render)
import OptParse
import CPM.ErrorLogger
import CPM.FileUtil ( fileInPath, joinSearchPath, safeReadFile, whenFileExists
                    , ifFileExists, inDirectory, removeDirectoryComplete )
import CPM.Config   ( Config ( packageInstallDir, binInstallDir
                             , binPackageDir, curryExec )
                    , readConfigurationWithDefault, showCompilerVersion )
import CPM.PackageCache.Global ( GlobalCache, readInstalledPackagesFromDir
                               , installFromZip, checkoutPackage
                               , uninstallPackage )
import CPM.Package
import CPM.Resolution ( isCompatibleToCompiler, showResult )
import CPM.Repository ( Repository, readRepository, findVersion, listPackages
                      , findLatestVersion, updateRepository, searchPackages )
import CPM.PackageCache.Runtime ( dependencyPathsSeparate, writePackageConfig )
import CPM.PackageCopy
import CPM.Diff.API as APIDiff
import qualified CPM.Diff.Behavior as BDiff
import CPM.ConfigPackage (packagePath)

-- Banner of this tool:
cpmBanner :: String
cpmBanner = unlines [bannerLine,bannerText,bannerLine]
 where
 bannerText =
  "Curry Package Manager <curry-language.org/tools/cpm> (version of 20/04/2017)"
 bannerLine = take (length bannerText) (repeat '-')

main :: IO ()
main = do
  args <- getArgs
  parseResult <- return $ parse (intercalate " " args) optionParser "cpm"
  case parseResult of
    Left err -> do putStrLn cpmBanner
                   putStrLn err
                   putStrLn "(use option -h for usage information)"
                   exitWith 1
    Right  r -> case applyParse r of
      Left err   -> do putStrLn cpmBanner
                       --printUsage "cpm" 80 optionParser
                       putStrLn err
                       exitWith 1
      Right opts -> runWithArgs opts

runWithArgs :: Options -> IO ()
runWithArgs opts = do
  missingExecutables <- checkExecutables
  unless (null missingExecutables) $ do
      putStrLn $ "The following programs could not be found on the PATH " ++
                 "(they are required for cpm to work):\n" ++
                 intercalate ", " missingExecutables
      exitWith 1
  config <- readConfigurationWithDefault (optDefConfig opts) >>= \c ->
   case c of
    Left err -> do putStrLn $ "Error reading .cpmrc settings: " ++ err
                   exitWith 1
    Right c' -> return c'
  let getGC   = getGlobalCache config
  let getRepo = getRepository config
  setLogLevel $ optLogLevel opts
  (msgs, result) <- case optCommand opts of
    NoCommand   -> failIO "NoCommand"
    Update      -> updateRepository config
    Compiler o  -> compiler o config getRepo getGC
    Exec o      -> exec     o config getRepo getGC
    Test o      -> test     o config getRepo getGC
    Clean       -> cleanPackage Info
    New o       -> newPackage o
    _ -> do repo <- getRepo
            case optCommand opts of
              List   o -> listCmd o config repo
              Search o -> search  o config repo
              _ -> do globalCache <- getGC
                      case optCommand opts of
                        Deps         -> deps         config repo globalCache
                        PkgInfo o    -> info       o config repo globalCache
                        Checkout o   -> checkout   o config repo globalCache
                        InstallBin o -> installbin o config repo globalCache
                        Install o    -> install    o config repo globalCache
                        Diff o       -> diff       o config repo globalCache
                        Uninstall o  -> uninstall  o config repo globalCache
                        Upgrade o    -> upgrade    o config repo globalCache
                        Link o       -> link       o config repo globalCache
                        _ -> error "Internal command processing error!"
  mapIO showLogEntry msgs
  let allOk =  all (levelGte Info) (map logLevelOf msgs) &&
               either (\le -> levelGte Info (logLevelOf le))
                      (const True)
                      result
  exitWith (if allOk then 0 else 1)

getGlobalCache :: Config -> IO GlobalCache
getGlobalCache config = do
  maybeGC <- readInstalledPackagesFromDir $ packageInstallDir config
  case maybeGC of
    Left err -> do putStrLn $ "Error reading global package cache: " ++ err
                   exitWith 1
    Right gc -> return gc

getRepository :: Config -> IO Repository
getRepository config = do
  (repo, repoErrors) <- readRepository config
  if null repoErrors
    then return repo
    else do putStrLn "Problems while reading the package index:"
            mapIO putStrLn repoErrors
            exitWith 1

data Options = Options
  { optLogLevel  :: LogLevel
  , optDefConfig :: [(String,String)]
  , optCommand   :: Command }

data Command 
  = Deps 
  | NoCommand
  | Checkout   CheckoutOptions
  | InstallBin CheckoutOptions
  | Install    InstallOptions
  | Uninstall  UninstallOptions
  | PkgInfo    InfoOptions
  | Compiler   CompilerOptions
  | Update
  | List       ListOptions
  | Search     SearchOptions
  | Upgrade    UpgradeOptions
  | Link       LinkOptions
  | Exec       ExecOptions
  | Test       TestOptions
  | Diff       DiffOptions
  | Clean
  | New        NewOptions

data CheckoutOptions = CheckoutOptions
  { coPackage    :: String
  , coVersion    :: Maybe Version
  , coPrerelease :: Bool }

data InstallOptions = InstallOptions
  { instTarget     :: Maybe String
  , instVersion    :: Maybe Version
  , instPrerelease :: Bool
  , instExecutable :: Bool }

data UninstallOptions = UninstallOptions
  { uninstPackage :: Maybe String
  , uninstVersion :: Maybe Version }

data InfoOptions = InfoOptions
  { infoPackage :: Maybe String
  , infoVersion :: Maybe Version
  , infoAll     :: Bool }

data ListOptions = ListOptions
  { listAll :: Bool   -- list all versions of each package
  , listCSV :: Bool   -- list in CSV format
  , listCat :: Bool   -- list all categories
  }

data SearchOptions = SearchOptions
  { searchQuery :: String }

data UpgradeOptions = UpgradeOptions
  { upgrTarget :: Maybe String }

data LinkOptions = LinkOptions
  { lnkSource :: String }

data NewOptions = NewOptions
  { projectName :: String }

data ExecOptions = ExecOptions
  { exeCommand :: String   -- command to be executed
  , exePath    :: [String] -- additional load path
  }

data CompilerOptions = CompilerOptions
  { comCommand :: String }

data TestOptions = TestOptions
  { testModules :: Maybe [String] }

data DiffOptions = DiffOptions
  { diffVersion  :: Maybe Version
  , diffModules  :: Maybe [String]
  , diffAPI      :: Bool
  , diffBehavior :: Bool
  , diffUseAna   :: Bool }

checkoutOpts :: Options -> CheckoutOptions
checkoutOpts s = case optCommand s of
  Checkout opts -> opts
  _             -> CheckoutOptions "" Nothing False

installOpts :: Options -> InstallOptions
installOpts s = case optCommand s of
  Install opts -> opts
  _            -> InstallOptions Nothing Nothing False True

uninstallOpts :: Options -> UninstallOptions
uninstallOpts s = case optCommand s of
  Uninstall opts -> opts
  _              -> UninstallOptions Nothing Nothing

infoOpts :: Options -> InfoOptions
infoOpts s = case optCommand s of
  PkgInfo opts -> opts
  _            -> InfoOptions Nothing Nothing False

listOpts :: Options -> ListOptions
listOpts s = case optCommand s of
  List opts -> opts
  _         -> ListOptions False False False

searchOpts :: Options -> SearchOptions
searchOpts s = case optCommand s of
  Search opts -> opts
  _           -> SearchOptions ""

upgradeOpts :: Options -> UpgradeOptions
upgradeOpts s = case optCommand s of
  Upgrade opts -> opts
  _            -> UpgradeOptions Nothing

linkOpts :: Options -> LinkOptions
linkOpts s = case optCommand s of
  Link opts -> opts
  _         -> LinkOptions ""

newOpts :: Options -> NewOptions
newOpts s = case optCommand s of
  New opts -> opts
  _        -> NewOptions ""

execOpts :: Options -> ExecOptions
execOpts s = case optCommand s of
  Exec opts -> opts
  _         -> ExecOptions "" []

compOpts :: Options -> CompilerOptions
compOpts s = case optCommand s of
  Compiler opts -> opts
  _             -> CompilerOptions ""

testOpts :: Options -> TestOptions
testOpts s = case optCommand s of
  Test opts -> opts
  _         -> TestOptions Nothing

diffOpts :: Options -> DiffOptions
diffOpts s = case optCommand s of
  Diff opts -> opts
  _         -> DiffOptions Nothing Nothing True True True

readLogLevel :: String -> Either String LogLevel
readLogLevel s = case map toLower s of
  "debug" -> Right Debug
  "info"  -> Right Info
  _       -> Left $ "Illegal verbosity value: " ++ s

readRcOption :: String -> Either String (String,String)
readRcOption s =
  let (option,value) = break (=='=') s
  in if null value then Left $ "Error in option definition: '=' missing"
                   else Right $ (option, tail value)

readVersion' :: String -> Either String Version
readVersion' s = case readVersion s of
  Nothing -> Left $ "'" ++ s ++ "' is not a valid version"
  Just  v -> Right v

applyEither :: [Options -> Either String Options] -> Options -> Either String Options
applyEither [] z = Right z
applyEither (f:fs) z = case f z of
  Left err -> Left err
  Right z' -> applyEither fs z'

applyParse :: [Options -> Either String Options] -> Either String Options
applyParse fs = applyEither fs defaultOpts
 where
  defaultOpts = Options Info [] NoCommand

(>.>) :: Either String a -> (a -> b) -> Either String b
a >.> f = case a of 
  Left err -> Left err
  Right  v -> Right $ f v

optionParser :: ParseSpec (Options -> Either String Options)
optionParser = optParser 
  (   option (\s a -> readLogLevel s >.> \ll -> a { optLogLevel = ll })
        (  long "verbosity"
        <> short "v"
        <> metavar "LEVEL"
        <> help "Log level for the application. Valid values are 'info' and 'debug'." )
  <.> option (\s a -> readRcOption s >.> \kv ->
                      a { optDefConfig = optDefConfig a ++ [kv] })
        (  long "define"
        <> short "d"
        <> metavar "DEFINITION"
        <> help "Overwrite definition of cpmrc file with 'option=value'." )
  <.> commands (metavar "COMMAND")
        (   command "checkout" (help "Checkout a package.") Right
              (   arg (\s a -> Right $ a { optCommand = Checkout
                                           (checkoutOpts a) { coPackage = s } })
                    (  metavar "PACKAGE"
                    <> help "The package name" )
              <.> arg (\s a -> readVersion' s >.> \v -> a { optCommand = Checkout (checkoutOpts a) { coVersion = Just v } })
                    (  metavar "VERSION"
                    <> help "The package version"
                    <> optional )
              <.> flag (\a -> Right $ a { optCommand = Checkout (checkoutOpts a) { coPrerelease = True } })
                    (  short "p"
                    <> long "pre"
                    <> help "Try pre-release versions when searching for newest version.") )
        <|> command "installbin" (help "Install the binary provided by a package.") 
                     Right
              (   arg (\s a -> Right $ a { optCommand = InstallBin
                                           (checkoutOpts a) { coPackage = s } })
                    (  metavar "PACKAGE"
                    <> help "The package name" )
              <.> arg (\s a -> readVersion' s >.> \v -> a { optCommand = InstallBin (checkoutOpts a) { coVersion = Just v } })
                    (  metavar "VERSION"
                    <> help "The package version"
                    <> optional)
              <.> flag (\a -> Right $ a { optCommand = InstallBin (checkoutOpts a) { coPrerelease = True } })
                    (  short "p"
                    <> long "pre"
                    <> help "Try pre-release versions when searching for newest version.") )
        <|> command "install" (help "Install a package.")
                     (\a -> Right $ a { optCommand = Install (installOpts a) })
              (   arg (\s a -> Right $ a { optCommand = Install (installOpts a) { instTarget = Just s } })
                    (  metavar "TARGET"
                    <> help "A package name or the path to a file"
                    <> optional)
              <.> arg (\s a -> readVersion' s >.> \v -> a { optCommand = Install (installOpts a) { instVersion = Just v } })
                    (  metavar "VERSION"
                    <> help "The package version"
                    <> optional)
              <.> flag (\a -> Right $ a { optCommand = Install (installOpts a) { instPrerelease = True } })
                    (  short "p"
                    <> long "pre"
                    <> help "Try pre-release versions when searching for newest version.")
              <.> flag (\a -> Right $ a { optCommand = Install (installOpts a) { instExecutable = False } })
                    (  short "n"
                    <> long "noexec"
                    <> help "Do not install executable.") )
        <|> command "uninstall" (help "Uninstall package")
                    (\a -> Right $ a { optCommand = Uninstall (uninstallOpts a) })
              (   arg (\s a -> Right $ a { optCommand =
                                Uninstall (uninstallOpts a) { uninstPackage = Just s } })
                    (  metavar "PACKAGE"
                    <> help "The package to be uninstalled"
                    <> optional)
              <.> arg (\s a -> readVersion' s >.> \v -> a { optCommand = Uninstall (uninstallOpts a) { uninstVersion = Just v } })
                    (  metavar "VERSION"
                    <> help "The version to be uninstalled"
                    <> optional) )
        <|> command "deps" (help "Calculate dependencies")
                           (\a -> Right $ a { optCommand = Deps }) [] 
        <|> command "clean" (help "Clean the current package")
                          (\a -> Right $ a { optCommand = Clean }) []
        <|> command "new" (help "Create a new package") Right
              ( arg (\s a -> Right $ a { optCommand = New (newOpts a)
                                                          { projectName = s } })
                    (  metavar "PROJECT"
                    <> help "The name of the new project" ) )
        <|> command "update" (help "Update the package index")
                             (\a -> Right $ a { optCommand = Update }) []
        <|> command "curry" (help "Load package spec from current directory and start Curry with correct dependencies.")
                            (\a -> Right $ a { optCommand = Compiler (compOpts a) })
              (   rest (\s a -> Right $ a { optCommand = Compiler (compOpts a) { comCommand = s } })
                    (  metavar "ARGS"
                    <> help "The options to pass to the compiler"
                    <> optional) )
        <|> command "exec" (help "Execute a command with the CURRYPATH set") (\a -> Right $ a { optCommand = Exec (execOpts a) })
              (   rest (\s a -> Right $ a { optCommand = Exec (execOpts a) { exeCommand = s } })
                    (  metavar "CMD"
                    <> help "The command to execute. Don't forget the quotes!"
                    <> optional) ) 
        <|> command "info" (help "Print package information")
                       (\a -> Right $ a { optCommand = PkgInfo (infoOpts a) })
              (   arg (\s a -> Right $ a { optCommand = PkgInfo (infoOpts a)
                                                    { infoPackage = Just s } })
                    (  metavar "PACKAGE"
                    <> help "The package name. If no name is specified, cpm tries to read a package specification in the current directory."
                    <> optional) 
              <.> arg (\s a -> readVersion' s >.> \v -> a
                                           { optCommand = PkgInfo (infoOpts a)
                                                    { infoVersion = Just v } })
                    (  metavar "VERSION"
                    <> help "The package version. If no version is specified, cpm uses the latest version of the specified package."
                    <> optional )
              <.> flag (\a -> Right $ a { optCommand = PkgInfo (infoOpts a)
                                                         { infoAll = True } })
                    (  short "a"
                    <> long "all"
                    <> help "Show all infos"
                    <> optional ) )
        <|> command "test" (help "Test the current package (with CurryCheck)")
                    (\a -> Right $ a { optCommand = Test (testOpts a) })
              (   option (\s a -> Right $ a { optCommand = Test (testOpts a)
                                     { testModules = Just $ splitOn "," s } })
                    (  long "modules"
                    <> short "m"
                    <> help
                "The modules to be tested, separate multiple modules by comma"
                    <> optional ) )
        <|> command "diff"
                    (help "Diff the current package against another version")
                    (\a -> Right $ a { optCommand = Diff (diffOpts a) })
              (   arg (\s a -> readVersion' s >.> \v -> a
                                { optCommand = Diff (diffOpts a)
                                                     { diffVersion = Just v } })
                    (  metavar "VERSION"
                    <> help "The other package version. If no version is specified, cpm diffs against the latest repository version."
                    <> optional )
              <.> option (\s a -> Right $ a { optCommand = Diff (diffOpts a)
                                      { diffModules = Just $ splitOn "," s } })
                    (  long "modules"
                    <> short "m"
                    <> help "The modules to compare, separate multiple modules by comma"
                    <> optional )
              <.> flag (\a -> Right $ a { optCommand = Diff (diffOpts a)
                                     { diffAPI = True, diffBehavior = False } })
                    (  long "api-only"
                    <> short "a"
                    <> help "Diff only the API")
              <.> flag (\a -> Right $ a { optCommand = Diff (diffOpts a)
                                     { diffAPI = False, diffBehavior = True } })
                    (  long "behavior-only"
                    <> short "b"
                    <> help "Diff only the behavior")
              <.> flag (\a -> Right $ a { optCommand = Diff (diffOpts a)
                                                       { diffUseAna = False } })
                    (  long "unsafe"
                    <> short "u"
                    <> help "Do not use automatic termination analysis for safe behavior checking") )
        <|> command "list" (help "List all packages of the repository")
                           (\a -> Right $ a { optCommand = List (listOpts a) })
              (   flag (\a -> Right $ a { optCommand = List (listOpts a)
                                                         { listAll = True } })
                    (  short "a"
                    <> long "all"
                    <> help "Show all versions" ) 
              <.> flag (\a -> Right $ a { optCommand = List (listOpts a)
                                                      { listCSV = True } })
                    (  short "t"
                    <> long "csv"
                    <> help "Show in CSV table format" )
              <.> flag (\a -> Right $ a { optCommand = List (listOpts a)
                                                      { listCat = True } })
                    (  short "c"
                    <> long "category"
                    <> help "Show all categories" )
              )
        <|> command "search" (help "Search the package repository") Right
              (   arg (\s a -> Right $ a { optCommand = Search (searchOpts a) { searchQuery = s } }) 
                    (  metavar "QUERY"
                    <> help "The search term" ) ) 
        <|> command "upgrade" (help "Upgrade one or more packages")
                    (\a -> Right $ a { optCommand = Upgrade (upgradeOpts a) })
              (   arg (\s a -> Right $ a { optCommand = Upgrade (upgradeOpts a) { upgrTarget = Just s } })
                    (  metavar "PACKAGE"
                    <> help "The package to upgrade" 
                    <> optional) )
        <|> command "link" (help "Link a package to the local cache") Right
              (   arg (\s a -> Right $ a { optCommand = Link (linkOpts a) { lnkSource = s } })
                    (  metavar "SOURCE"
                    <> help "The directory to link" ) ) ) )

-- Check if operating system executables we depend on are present on the current
-- system.
checkExecutables :: IO [String]
checkExecutables = do
  present <- mapIO fileInPath listOfExecutables
  return $ map fst $ filter (not . snd) (zip listOfExecutables present)
 where
  listOfExecutables = 
    [ "curl"  
    , "git"   
    , "unzip" 
    , "cp"
    , "rm"
    , "ln"
    , "readlink" ]

deps :: Config -> Repository -> GlobalCache -> IO (ErrorLogger ())
deps cfg repo gc =
  tryFindLocalPackageSpec "." |>= \specDir ->
  resolveDependencies cfg repo gc specDir |>= \result ->
  putStrLn (showResult result) >> succeedIO ()

info :: InfoOptions -> Config -> Repository -> GlobalCache
     -> IO (ErrorLogger ())
info (InfoOptions Nothing Nothing allinfos) _ repo gc =
  tryFindLocalPackageSpec "." |>= \specDir ->
  loadPackageSpec specDir |>= printInfo allinfos repo gc
info (InfoOptions (Just pkg) Nothing allinfos) cfg repo gc =
  case findLatestVersion cfg repo pkg False of
   Nothing -> failIO $
                "Package '" ++ pkg ++ "' not found in package repository."
   Just p  -> printInfo allinfos repo gc p
info (InfoOptions (Just pkg) (Just v) allinfos) _ repo gc =
 case findVersion repo pkg v of
   Nothing -> failIO $ "Package '" ++ pkg ++ "-" ++ (showVersion v) ++
                       "' not found in package repository."
   Just p  -> printInfo allinfos repo gc p
info (InfoOptions Nothing (Just _) _) _ _ _ = failIO "Must specify package name"

printInfo :: Bool -> Repository -> GlobalCache -> Package -> IO (ErrorLogger ())
printInfo allinfos repo gc pkg =
  putStrLn (renderPackageInfo allinfos repo gc pkg) >> succeedIO ()


compiler :: CompilerOptions -> Config -> IO Repository -> IO GlobalCache
         -> IO (ErrorLogger ())
compiler o cfg getRepo getGC =
  tryFindLocalPackageSpec "." |>= \pkgdir ->
  loadPackageSpec pkgdir |>= \pkg ->
  checkCompiler cfg pkg >>
  loadCurryPathFromCache pkgdir |>=
  maybe (computePackageLoadPath pkgdir pkg) succeedIO |>= \currypath ->
  log Info ("Starting '" ++ currybin ++ "' with") |>
  log Info ("CURRYPATH=" ++ currypath) |>
  do setEnviron "CURRYPATH" $ currypath
     ecode <- system $ currybin ++ " " ++ comCommand o
     unsetEnviron "CURRYPATH"
     unless (ecode==0) (exitWith ecode)
     succeedIO ()
 where
  currybin = curryExec cfg

  computePackageLoadPath pkgdir pkg =
    getRepo >>= \repo ->
    getGC >>= \gc ->
    resolveAndCopyDependenciesForPackage cfg repo gc pkgdir pkg |>= \pkgs ->
    getAbsolutePath pkgdir >>= \abs -> succeedIO () |>
    let srcdirs = map (abs </>) (sourceDirsOf pkg)
        currypath = joinSearchPath (srcdirs ++ dependencyPathsSeparate pkgs abs)
    in saveCurryPathToCache pkgdir currypath >> succeedIO currypath


checkout :: CheckoutOptions -> Config -> Repository -> GlobalCache
         -> IO (ErrorLogger ())
checkout (CheckoutOptions pkg Nothing pre) cfg repo gc =
 case findLatestVersion cfg repo pkg pre of
  Nothing -> failIO $ "Package '" ++ pkg ++
                      "' not found in package repository."
  Just  p -> acquireAndInstallPackageWithDependencies cfg repo gc p |>
             checkoutPackage cfg repo gc p
checkout (CheckoutOptions pkg (Just ver) _) cfg repo gc =
 case findVersion repo pkg ver of
  Nothing -> failIO $ "Package '" ++ pkg ++ "-" ++ showVersion ver ++
                      "' not found in package repository."
  Just  p -> acquireAndInstallPackageWithDependencies cfg repo gc p |>
             checkoutPackage cfg repo gc p

--- Installs the binary provided by a package.
--- This is done by checking out the package into CPM's bin_packages
--- cache (default: $HOME/.cpm/bin_packages, see bin_package_path
--- in .cpmrc configuration file) and then install this package.
---
--- Note: the installed package should not be cleaned or removed
--- after the installation since its execution might refer (via the
--- config module) to some data stored in the package.
installbin :: CheckoutOptions -> Config -> Repository -> GlobalCache
           -> IO (ErrorLogger ())
installbin opts cfg repo gc = do
  removeDirectoryComplete copkgdir
  debugMessage ("Change into directory " ++ binpkgdir)
  inDirectory binpkgdir
    (checkout opts cfg repo gc |>
     log Debug ("Change into directory " ++ copkgdir) |>
     (setCurrentDirectory copkgdir >> succeedIO ()) |>
     install (InstallOptions Nothing Nothing False True) cfg repo gc )
 where
  binpkgdir = binPackageDir cfg
  copkgdir  = binpkgdir </> coPackage opts

install :: InstallOptions -> Config -> Repository -> GlobalCache
        -> IO (ErrorLogger ())
install (InstallOptions Nothing Nothing _ instexec) cfg repo gc =
  tryFindLocalPackageSpec "." |>= \pkgdir ->
  cleanCurryPathCache pkgdir |>
  installLocalDependencies cfg repo gc pkgdir |>= \ (pkg,_) ->
  if instexec then installExecutable cfg repo pkg pkgdir else succeedIO ()
install (InstallOptions (Just pkg) Nothing pre _) cfg repo gc = do
  fileExists <- doesFileExist pkg
  if fileExists
    then installFromZip cfg pkg
    else case findLatestVersion cfg repo pkg pre of
      Nothing -> failIO $ "Package '" ++ pkg ++
                          "' not found in package repository."
      Just  p -> acquireAndInstallPackageWithDependencies cfg repo gc p
install (InstallOptions (Just pkg) (Just ver) _ _) cfg repo gc =
 case findVersion repo pkg ver of
  Nothing -> failIO $ "Package '" ++ pkg ++ "-" ++ (showVersion ver) ++
                      "' not found in package repository."
  Just  p -> acquireAndInstallPackageWithDependencies cfg repo gc p
install (InstallOptions Nothing (Just _) _ _) _ _ _ =
  failIO "Must specify package name"

--- Checks the compiler compatibility.
checkCompiler :: Config -> Package -> IO ()
checkCompiler cfg pkg =
  unless (isCompatibleToCompiler cfg pkg)
    (error $ "Incompatible compiler: " ++ showCompilerVersion cfg)

--- Installs the executable specified in the package in the
--- bin directory of CPM (compare .cpmrc).
installExecutable :: Config -> Repository -> Package -> String
                  -> IO (ErrorLogger ())
installExecutable cfg repo pkg pkgdir =
  checkCompiler cfg pkg >>
  -- we read the global cache again since it might be modified by
  -- the installation of the package:
  getGlobalCache cfg >>= \gc ->
  maybe (succeedIO ())
        (\ (PackageExecutable name mainmod) ->
           getLogLevel >>= \lvl ->
           getEnviron "PATH" >>= \path ->
           log Info ("Compiling main module: " ++ mainmod) |>
           let cmd = unwords $
                       [":set", if levelGte Debug lvl then "v1" else "v0",
                        ":load", mainmod, ":save", ":quit"]
               bindir     = binInstallDir cfg
               binexec    = bindir </> name
           in writePackageConfig cfg pkgdir pkg |>
              compiler CompilerOptions { comCommand = cmd }
                       cfg (return repo) (return gc) |>
              log Info ("Installing executable '" ++ name ++ "' into '" ++
                        bindir ++ "'") |>
              (whenFileExists binexec (backupExistingBin binexec) >>
               -- renaming might not work across file systems, hence we move:
               system (unwords ["mv", mainmod, binexec]) >>
               checkPath path bindir))
        (executableSpec pkg)
 where
  backupExistingBin binexec = do
    let binexecbak = binexec ++ ".bak"
    system $ "rm -f " ++ binexecbak
    renameFile binexec binexecbak
    infoMessage $ "Existing executable '" ++ binexec ++ "' saved to '" ++
                  binexecbak ++ "'."

  checkPath path bindir =
    if bindir `elem` splitSearchPath path
      then succeedIO ()
      else log Info $ "It is recommended to add '" ++bindir++ "' to your path!"


uninstall :: UninstallOptions -> Config -> Repository -> GlobalCache
          -> IO (ErrorLogger ())
uninstall (UninstallOptions (Just pkg) (Just ver)) cfg repo gc =
  uninstallPackage cfg repo gc pkg ver
uninstall (UninstallOptions (Just _) Nothing) _ _ _ =
  log Error "Please provide a package and version number!"
uninstall (UninstallOptions Nothing (Just _)) _ _ _ =
  log Error "Please provide a package and version number!"
uninstall (UninstallOptions Nothing Nothing) cfg _ _ =
  tryFindLocalPackageSpec "." |>= \pkgdir ->
  loadPackageSpec pkgdir |>= \pkg ->
  maybe (succeedIO ())
        (\ (PackageExecutable name _) ->
           let binexec = binInstallDir cfg </> name
           in ifFileExists binexec
                (removeFile binexec >>
                 log Info ("Executable '" ++ binexec ++ "' removed"))
                (log Info $ "Executable '" ++ binexec ++ "' not installed"))
        (executableSpec pkg)

tryFindVersion :: String -> Version -> Repository -> IO (ErrorLogger Package)
tryFindVersion pkg ver repo = case findVersion repo pkg ver of
  Nothing -> failIO $ "Package '" ++ pkg ++ "-" ++ (showVersion ver) ++
                      "' not found in package repository."
  Just  p -> succeedIO $ p

--- Lists all (compiler-compatible) packages in the given repository.
listCmd :: ListOptions -> Config -> Repository -> IO (ErrorLogger ())
listCmd (ListOptions lv csv cat) cfg repo =
  if cat then putStr (renderCats catgroups) >> succeedIO ()
         else putStr (renderPkgs allpkgs)   >> succeedIO ()
 where
  -- all packages (and versions if `lv`)
  allpkgs = concatMap (if lv then id else ((:[]) . head))
                      (sortBy (\ps1 ps2 -> name (head ps1) <= name (head ps2))
                              (listPackages cfg repo))

  -- all categories together with their package names:
  catgroups =
    let pkgid p = name p ++ '-' : showVersion (version p)
        newpkgs = map head (listPackages cfg repo)
        catpkgs = concatMap (\p -> map (\c -> (c, pkgid p)) (category p))
                            newpkgs
        nocatps = map pkgid (filter (null . category) newpkgs)
    in map (\cg -> (fst (head cg), map snd cg))
           (groupBy (\ (c1,_) (c2,_) -> c1==c2) (nub $ sortBy (<=) catpkgs)) ++
       if null nocatps then []
                       else [("???", nub $ sortBy (<=) nocatps)]

  renderPkgs pkgs =
    let (colsizes,rows) = packageVersionAsTable pkgs
    in renderTable colsizes rows

  renderCats catgrps =
    let namelen = foldl max 8 $ map (length . fst) catgrps
        header = [ ["Category", "Packages"]
                 , ["--------", "--------"]]
        rows   = header ++ map (\ (c,ns) -> [c, unwords ns]) catgrps
    in renderTable [namelen + 4, 76 - namelen] rows
  
  renderTable colsizes rows =
    if csv then showCSV (head rows : drop 2 rows)
           else unlines [render (table rows colsizes), cpmInfo, cpmUpdate]

-- Format a list of packages by showing their names, synopsis, and versions
-- as table rows. Returns also the column sizes.
packageVersionAsTable :: [Package] -> ([Int],[[String]])
packageVersionAsTable pkgs = (colsizes, rows)
 where
  namelen = foldl max 4 $ map (length . name) pkgs
  colsizes = [namelen + 4, 66 - namelen, 10]
  header  = [ ["Name", "Synopsis", "Version"]
            , ["----", "--------", "-------"]]
  rows    = header ++ map formatPkg pkgs
  formatPkg p = [name p, synopsis p, showVersion (version p)]
        
cpmInfo :: String
cpmInfo = "Use 'cpm info PACKAGE' for more information about a package."

cpmUpdate :: String
cpmUpdate = "Use 'cpm update' to download the newest package index."


--- Search in all (compiler-compatible) packages in the given repository.
search :: SearchOptions -> Config -> Repository -> IO (ErrorLogger ())
search (SearchOptions q) cfg repo = putStr rendered >> succeedIO ()
 where
  results = sortBy (\p1 p2 -> name p1 <= name p2) (searchPackages cfg repo q)
  (colsizes,rows) = packageVersionAsTable results
  rendered = unlines $
               if null results
                 then ["No packages found for '" ++ q, "", cpmUpdate]
                 else [ render (table rows colsizes), cpmInfo, cpmUpdate ]

upgrade :: UpgradeOptions -> Config -> Repository -> GlobalCache
        -> IO (ErrorLogger ())
upgrade (UpgradeOptions Nothing) cfg repo gc =
  tryFindLocalPackageSpec "." |>= \specDir ->
  cleanCurryPathCache specDir |>
  log Info "Upgrading all packages" |>
  upgradeAllPackages cfg repo gc specDir
upgrade (UpgradeOptions (Just pkg)) cfg repo gc =
  tryFindLocalPackageSpec "." |>= \specDir ->
  log Info ("Upgrade " ++ pkg) |>
  upgradeSinglePackage cfg repo gc specDir pkg


link :: LinkOptions -> Config -> Repository -> GlobalCache
     -> IO (ErrorLogger ())
link (LinkOptions src) _ _ _ =
  tryFindLocalPackageSpec "." |>= \specDir ->
  cleanCurryPathCache specDir |>
  log Info ("Linking '" ++ src ++ "' into local package cache") |>
  linkToLocalCache src specDir

--- `test` command: run `curry check` on exported or top-level modules
--- of the package, or on the modules provided as an argument.
test :: TestOptions -> Config -> IO Repository -> IO GlobalCache
     -> IO (ErrorLogger ())
test opts cfg getRepo getGC =
  tryFindLocalPackageSpec "." |>= \specDir ->
  loadPackageSpec specDir |>= \pkg -> do
    checkCompiler cfg pkg
    aspecDir <- getAbsolutePath specDir
    mainprogs <- curryModulesInDir (aspecDir </> "src")
    let tests = testsuites pkg mainprogs
    if null tests
      then putStrLn "No modules to be tested!" >> succeedIO ()
      else foldEL (\_ -> execTest aspecDir) () tests
 where
  currycheck = curryExec cfg ++ " check"
  
  execTest apkgdir (PackageTest dir mods ccopts script) = do
    let scriptcmd = "CURRYBIN=" ++ curryExec cfg ++ " && export CURRYBIN && " ++
                    "." </> script ++ if null ccopts then "" else ' ' : ccopts
        checkcmd  = currycheck ++ if null ccopts then "" else ' ' : ccopts
    unless (null mods) $ putStrLn $
      "Running CurryCheck (" ++ checkcmd ++ ")\n" ++
      "(in directory '" ++ dir ++ "', showing raw output) on modules:\n" ++
      unwords mods ++ "\n"
    unless (null script) $ putStrLn $
      "Executing test script with command:\n" ++ scriptcmd ++ "\n" ++
      "(in directory '" ++ dir ++ "', showing raw output):\n"
    let currysubdir = apkgdir </> addCurrySubdir dir
        testcmd = if not (null mods)
                    then unwords (checkcmd : mods)
                    else scriptcmd
    debugMessage $ "Removing directory: " ++ currysubdir
    system (unwords ["rm", "-rf", currysubdir])
    inDirectory (apkgdir </> dir) $
      execWithPkgDir (ExecOptions testcmd []) cfg getRepo getGC apkgdir

  testsuites spec mainprogs = case testModules opts of
    Nothing -> maybe (let exports = exportedModules spec
                      in if null exports
                           then if null mainprogs
                                  then []
                                  else [PackageTest "src" mainprogs "" ""]
                           else [PackageTest "src" exports "" ""])
                     id
                     (testSuite spec)
    Just ms -> [PackageTest "src" ms "" ""]

--- Get the names of all Curry modules containing in a directory.
--- Modules in subdirectories are returned as hierarchical modules.
curryModulesInDir :: String -> IO [String]
curryModulesInDir dir = getModules "" dir
 where
  getModules p d = do
    entries <- getDirectoryContents d
    let realentries = filter (\f -> length f >= 1 && head f /= '.') entries
        newprogs    = filter (\f -> takeExtension f == ".curry") realentries
    subdirs <- mapIO (\e -> doesDirectoryExist (d </> e) >>=
                            \b -> return $ if b then [e] else []) realentries
               >>= return . concat
    subdirentries <- mapIO (\s -> getModules (p ++ s ++ ".") (d </> s)) subdirs
    return $ map ((p ++) . stripCurrySuffix) newprogs ++ concat subdirentries


diff :: DiffOptions -> Config -> Repository -> GlobalCache
     -> IO (ErrorLogger ())
diff opts cfg repo gc =
  tryFindLocalPackageSpec "." |>= \specDir ->
  loadPackageSpec specDir     |>= \localSpec ->
  let localname  = name localSpec
      localv     = version localSpec
      showlocalv = showVersion localv
  in
  getDiffVersion localname |>= \diffv ->
  if diffv == localv
    then failIO $ "Cannot diff identical package versions " ++ showlocalv
    else putStrLn ("Comparing local version " ++ showlocalv ++
                   " and repository version " ++ showVersion diffv ++ ":\n") >>
         diffAPIIfEnabled      specDir localSpec diffv |> 
         diffBehaviorIfEnabled specDir localSpec diffv
 where
  getDiffVersion localname = case diffVersion opts of
    Nothing -> case findLatestVersion cfg repo localname False of
                 Nothing -> failIO $ "No other version of local package '" ++
                                 localname ++ "' found in package repository."
                 Just p  -> succeedIO (version p)
    Just v  -> succeedIO v
 
  diffAPIIfEnabled specDir localSpec diffversion =
    if diffAPI opts
    then (putStrLn "Running API diff...\n" >> succeedIO ()) |>
         APIDiff.compareModulesFromPackageAndDir cfg repo gc specDir
                          (name localSpec) diffversion (diffModules opts) |>=
         \diffResults ->
         let diffOut = APIDiff.showDifferences (map snd diffResults)
                                             (version localSpec) diffversion
         in unless (null diffOut) (putStrLn diffOut >> putStrLn "") >>
            succeedIO ()
    else succeedIO () 

  diffBehaviorIfEnabled specDir localSpec diffversion =
    if diffBehavior opts
      then (putStrLn "Preparing behavior diff...\n" >> succeedIO ()) |>
           BDiff.preparePackageAndDir cfg repo gc specDir (name localSpec)
                                                          diffversion |>=
           \i -> BDiff.diffBehavior cfg repo gc i (diffUseAna opts)
                                    (diffModules opts)
      else succeedIO ()


exec :: ExecOptions -> Config -> IO Repository -> IO GlobalCache
     -> IO (ErrorLogger ())
exec o cfg getRepo getGC =
  tryFindLocalPackageSpec "." |>= execWithPkgDir o cfg getRepo getGC

execWithPkgDir :: ExecOptions -> Config -> IO Repository -> IO GlobalCache
               -> String -> IO (ErrorLogger ())
execWithPkgDir o cfg getRepo getGC specDir =
  loadCurryPathFromCache specDir |>=
  maybe (computePackageLoadPath specDir) succeedIO |>= \currypath ->
  let execpath = joinSearchPath (exePath o ++ splitSearchPath currypath)
  in log Debug ("Setting CURRYPATH to " ++ execpath) |>
  do setEnviron "CURRYPATH" execpath
     ecode <- system (exeCommand o)
     unsetEnviron "CURRYPATH"
     unless (ecode==0) (exitWith ecode)
     succeedIO ()
 where
  computePackageLoadPath pkgdir =
    getRepo >>= \repo -> getGC >>= \gc ->
    loadPackageSpec pkgdir |>= \pkg ->
    resolveAndCopyDependenciesForPackage cfg repo gc pkgdir pkg |>= \pkgs ->
    getAbsolutePath pkgdir >>= \abs -> succeedIO () |>
    let srcdirs = map (abs </>) (sourceDirsOf pkg)
        currypath = joinSearchPath (srcdirs ++ dependencyPathsSeparate pkgs abs)
    in saveCurryPathToCache pkgdir currypath >> succeedIO currypath


-- Clean auxiliary files in the current package
cleanPackage :: LogLevel -> IO (ErrorLogger ())
cleanPackage ll =
  tryFindLocalPackageSpec "." |>= \specDir ->
  loadPackageSpec specDir     |>= \pkg ->
  let dotcpm   = specDir </> ".cpm"
      srcdirs  = map (specDir </>) (sourceDirsOf pkg)
      testdirs = map (specDir </>)
                     (maybe []
                            (map (\ (PackageTest m _ _ _) -> m))
                            (testSuite pkg))
      rmdirs   = dotcpm : map addCurrySubdir (srcdirs ++ testdirs)
  in log ll ("Removing directories: " ++ unwords rmdirs) |>
     (system (unwords (["rm", "-rf"] ++ rmdirs)) >> succeedIO ())

--- Creates a new package by asking some questions.
newPackage :: NewOptions -> IO (ErrorLogger ())
newPackage (NewOptions pname) = do
  exists <- doesDirectoryExist pname
  when exists $ do
    putStrLn $ "There is already a directory with the new project name. " ++
               "I cannot create new project!"
    exitWith 1
  let emptyAuthor   = "YOUR NAME <YOUR EMAIL ADDRESS>"
      emptySynopsis = "PLEASE PROVIDE A ONE-LINE SUMMARY ABOUT THE PACKAGE"
  createDirectory pname
  let pkgSpec = emptyPackage { name            = pname
                             , version         = initialVersion
                             , author          = emptyAuthor
                             , synopsis        = emptySynopsis
                             , category        = ["Programming"]
                             , dependencies    = [] 
                             , exportedModules = []
                             , license         = Just "BSD-3-Clause"
                             , licenseFile     = Just "LICENSE"
                             }
  writePackageSpec pkgSpec (pname </> "package.json")
  copyFile (packagePath </> "templates" </> "LICENSE") (pname </> "LICENSE")
  createDirectory (pname </> "src")
  let cmain = "Main.curry"
  copyFile (packagePath </> "templates" </> cmain) (pname </> "src" </> cmain)
  writeFile (pname </> "README.md") readme
  putStr $ unlines todo
  succeedIO ()
 where
  readme = unlines [pname, take (length pname) (repeat '=')]

  todo =
    [ "A new package in the directory '" ++ pname ++ "' has been created!"
    , ""
    , "Please go into this directory and edit the file 'package.json':"
    , "- enter correct values for the fields 'author', 'synopsis', 'category'"
    , "- add dependencies in the field 'dependencies'"
    , "- add further fields (e.g., 'description')"
    , "- review field 'license' (and adapt file 'LICENSE')"
    , ""
    , "Then run 'cpm install' to install all dependencies and"
    , "put your program code in directory 'src'"
    , "(where you find a template file 'Main.curry')"
    , ""
    , "Run the main program with:"
    , "> cpm curry :load Main :eval main :quit"
    ]

---------------------------------------------------------------------------
-- Caching the current CURRYPATH of a package for faster startup.

--- The name of the cache file in a package directory.
curryPathCacheFile :: String -> String
curryPathCacheFile pkgdir = pkgdir </> ".cpm" </> "currypath_cache"

--- Saves package CURRYPATH in local cache file in the given package dir.
saveCurryPathToCache :: String -> String -> IO ()
saveCurryPathToCache pkgdir path = do
  let cpmdir = pkgdir </> ".cpm"
  createDirectoryIfMissing False cpmdir
  writeFile (curryPathCacheFile pkgdir) (path ++ "\n")

--- Restores package CURRYPATH from local cache file in the given package dir,
--- if it is still up-to-date, i.e., it exists and is newer than the package
--- specification.
loadCurryPathFromCache :: String -> IO (ErrorLogger (Maybe String))
loadCurryPathFromCache pkgdir = do
  let cachefile = curryPathCacheFile pkgdir
  excache <- doesFileExist cachefile
  if excache
    then do
      cftime <- getModificationTime cachefile
      pftime <- getModificationTime (pkgdir </> "package.json")
      if cftime > pftime
        then do cnt <- safeReadFile cachefile
                let ls = either (const []) lines cnt
                succeedIO $ if null ls then Nothing
                                       else Just (head ls)
        else succeedIO Nothing
    else succeedIO Nothing

--- Cleans the local cache file for CURRYPATH. This might be necessary
--- for upgrade/install/link commands.
cleanCurryPathCache :: String -> IO (ErrorLogger ())
cleanCurryPathCache pkgdir = do
  let cachefile = curryPathCacheFile pkgdir
  whenFileExists cachefile $ removeFile cachefile
  succeedIO ()

---------------------------------------------------------------------------
