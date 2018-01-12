--------------------------------------------------------------------------------
--- This is the main module of the Curry Package Manager.
--------------------------------------------------------------------------------

module CPM.Main where

import Char         ( toLower )
import Directory    ( doesFileExist, getAbsolutePath, doesDirectoryExist
                    , copyFile, createDirectory, createDirectoryIfMissing
                    , getCurrentDirectory, getDirectoryContents
                    , getModificationTime
                    , renameFile, removeFile, setCurrentDirectory )
import Distribution ( installDir, stripCurrySuffix, addCurrySubdir )
import Either
import FilePath     ( (</>), splitSearchPath, replaceExtension, takeExtension
                    , pathSeparator, isPathSeparator )
import IO           ( hFlush, stdout )
import List         ( groupBy, intercalate, isPrefixOf, isSuffixOf, nub, split
                    , splitOn )
import Sort         ( sortBy )
import System       ( getArgs, getEnviron, setEnviron, unsetEnviron, exitWith
                    , system )

import Boxes        ( table, render )
import OptParse
import Text.CSV     ( showCSV )

import CPM.ErrorLogger
import CPM.FileUtil ( fileInPath, joinSearchPath, safeReadFile, whenFileExists
                    , ifFileExists, inDirectory, removeDirectoryComplete
                    , copyDirectory, quote )
import CPM.Config   ( Config (..)
                    , readConfigurationWith, showCompilerVersion
                    , showConfiguration )
import CPM.PackageCache.Global ( GlobalCache, readGlobalCache, allPackages
                               , installFromZip, checkoutPackage
                               , uninstallPackage, packageInstalled )
import CPM.Package
import CPM.Resolution ( isCompatibleToCompiler, showResult )
import CPM.Repository ( Repository, readRepository, findVersion, listPackages
                      , findAllVersions, findLatestVersion, updateRepository
                      , useUpdateHelp, searchPackages, cleanRepositoryCache
                      , readPackageFromRepository
                      , getAllPackageVersions, getPackageVersion )
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
  "Curry Package Manager <curry-language.org/tools/cpm> (version of 12/01/2018)"
 bannerLine = take (length bannerText) (repeat '-')

main :: IO ()
main = do
  args <- getArgs
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
  setLogLevel $ optLogLevel opts
  setWithShowTime (optWithTime opts)
  debugMessage "Reading CPM configuration..."
  config <- readConfigurationWith (optDefConfig opts) >>= \c ->
   case c of
    Left err -> do putStrLn $ "Error reading .cpmrc settings: " ++ err
                   exitWith 1
    Right c' -> return c'
  debugMessage ("Current configuration:\n" ++ showConfiguration config)
  (msgs, result) <- case optCommand opts of
    NoCommand   -> failIO "NoCommand"
    Config o    -> configCmd o config
    Update      -> updateCmd   config
    Compiler o  -> compiler  o config
    Exec o      -> execCmd   o config
    Doc  o      -> docCmd    o config
    Test o      -> testCmd   o config
    Uninstall o -> uninstall o config
    Deps o      -> depsCmd   o config
    PkgInfo   o -> infoCmd   o config
    Link o      -> linkCmd   o config
    Add  o      -> addCmd    o config
    Clean       -> cleanPackage config Info
    New o       -> newPackage o
    cmd -> do repo <- readRepository config (cmdWithLargeRepoCache cmd)
              case optCommand opts of
                List     o -> listCmd     o config repo
                Search   o -> searchCmd   o config repo
                Checkout o -> checkoutCmd o config repo
                Install  o -> installCmd  o config repo
                Upgrade  o -> upgradeCmd  o config repo
                Diff     o -> diffCmd     o config repo
                _ -> error "Internal command processing error!"
  mapIO showLogEntry msgs
  let allOk =  all (levelGte Info) (map logLevelOf msgs) &&
               either (\le -> levelGte Info (logLevelOf le))
                      (const True)
                      result
  exitWith (if allOk then 0 else 1)
 where
  cmdWithLargeRepoCache cmd =
    case cmd of List   _ -> True
                Search _ -> True
                _        -> False

data Options = Options
  { optLogLevel  :: LogLevel
  , optDefConfig :: [(String,String)]
  , optWithTime  :: Bool
  , optCommand   :: Command }

data Command 
  = NoCommand
  | Config     ConfigOptions
  | Deps       DepsOptions
  | Checkout   CheckoutOptions
  | Install    InstallOptions
  | Uninstall  UninstallOptions
  | PkgInfo    InfoOptions
  | Compiler   ExecOptions
  | Update
  | List       ListOptions
  | Search     SearchOptions
  | Upgrade    UpgradeOptions
  | Link       LinkOptions
  | Add        AddOptions
  | Exec       ExecOptions
  | Doc        DocOptions
  | Test       TestOptions
  | Diff       DiffOptions
  | New        NewOptions
  | Clean

data ConfigOptions = ConfigOptions
  { configAll :: Bool  -- show also installed packages?
  }

data DepsOptions = DepsOptions
  { depsPath :: Bool  -- show CURRYPATH only?
  }

data CheckoutOptions = CheckoutOptions
  { coPackage    :: String
  , coVersion    :: Maybe Version
  , coPrerelease :: Bool }

data InstallOptions = InstallOptions
  { instTarget     :: Maybe String
  , instVersion    :: Maybe Version
  , instPrerelease :: Bool
  , instExecutable :: Bool
  , instExecOnly   :: Bool }

data UninstallOptions = UninstallOptions
  { uninstPackage :: Maybe String
  , uninstVersion :: Maybe Version }

data InfoOptions = InfoOptions
  { infoPackage :: Maybe String
  , infoVersion :: Maybe Version
  , infoAll     :: Bool
  , infoPlain   :: Bool  -- plain output, no bold/color
  }

data ListOptions = ListOptions
  { listVers :: Bool   -- list all versions of each package
  , listCSV  :: Bool   -- list in CSV format
  , listCat  :: Bool   -- list all categories
  }

data SearchOptions = SearchOptions
  { searchQuery  :: String -- the term to search for
  , searchModule :: Bool   -- search for some module?
  , searchExec   :: Bool   -- search for some executable?
  }

data UpgradeOptions = UpgradeOptions
  { upgrTarget :: Maybe String }

data LinkOptions = LinkOptions
  { lnkSource :: String }

data AddOptions = AddOptions
  { addPackage    :: Bool
  , addDependency :: Bool
  , addSource     :: String
  , forceAdd      :: Bool
  }

data NewOptions = NewOptions
  { projectName :: String }

data ExecOptions = ExecOptions
  { exeCommand :: String   -- the command to be executed
  }

data DocOptions = DocOptions
  { docDir        :: Maybe String    -- documentation directory
  , docModules    :: Maybe [String]  -- modules to be documented
  , docPrograms   :: Bool            -- generate documentation for programs
  , docManual     :: Bool            -- generate manual (if specified)
  , docGenImports :: Bool            -- generate documentation for imported pkgs
                                     -- (otherwise, use their standard docs)
  }

data TestOptions = TestOptions
  { testModules :: Maybe [String] }

data DiffOptions = DiffOptions
  { diffVersion  :: Maybe Version
  , diffModules  :: Maybe [String]
  , diffAPI      :: Bool
  , diffBehavior :: Bool
  , diffUseAna   :: Bool }

configOpts :: Options -> ConfigOptions
configOpts s = case optCommand s of
  Config opts -> opts
  _           -> ConfigOptions False

depsOpts :: Options -> DepsOptions
depsOpts s = case optCommand s of
  Deps opts -> opts
  _         -> DepsOptions False

checkoutOpts :: Options -> CheckoutOptions
checkoutOpts s = case optCommand s of
  Checkout opts -> opts
  _             -> CheckoutOptions "" Nothing False

installOpts :: Options -> InstallOptions
installOpts s = case optCommand s of
  Install opts -> opts
  _            -> InstallOptions Nothing Nothing False True False

uninstallOpts :: Options -> UninstallOptions
uninstallOpts s = case optCommand s of
  Uninstall opts -> opts
  _              -> UninstallOptions Nothing Nothing

infoOpts :: Options -> InfoOptions
infoOpts s = case optCommand s of
  PkgInfo opts -> opts
  _            -> InfoOptions Nothing Nothing False False

listOpts :: Options -> ListOptions
listOpts s = case optCommand s of
  List opts -> opts
  _         -> ListOptions False False False

searchOpts :: Options -> SearchOptions
searchOpts s = case optCommand s of
  Search opts -> opts
  _           -> SearchOptions "" False False

upgradeOpts :: Options -> UpgradeOptions
upgradeOpts s = case optCommand s of
  Upgrade opts -> opts
  _            -> UpgradeOptions Nothing

linkOpts :: Options -> LinkOptions
linkOpts s = case optCommand s of
  Link opts -> opts
  _         -> LinkOptions ""

addOpts :: Options -> AddOptions
addOpts s = case optCommand s of
  Add opts -> opts
  _        -> AddOptions False False "" False

newOpts :: Options -> NewOptions
newOpts s = case optCommand s of
  New opts -> opts
  _        -> NewOptions ""

execOpts :: Options -> ExecOptions
execOpts s = case optCommand s of
  Exec opts -> opts
  _         -> ExecOptions ""

docOpts :: Options -> DocOptions
docOpts s = case optCommand s of
  Doc opts -> opts
  _        -> DocOptions Nothing Nothing True True False

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
  "quiet" -> Right Quiet
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

applyEither :: [Options -> Either String Options] -> Options
            -> Either String Options
applyEither [] z = Right z
applyEither (f:fs) z = case f z of
  Left err -> Left err
  Right z' -> applyEither fs z'

applyParse :: [Options -> Either String Options] -> Either String Options
applyParse fs = applyEither fs defaultOpts
 where
  defaultOpts = Options Info [] False NoCommand

(>.>) :: Either String a -> (a -> b) -> Either String b
a >.> f = case a of 
  Left err -> Left err
  Right  v -> Right $ f v

optionParser :: [String] -> ParseSpec (Options -> Either String Options)
optionParser allargs = optParser 
  (   option (\s a -> readLogLevel s >.> \ll -> a { optLogLevel = ll })
       (  long "verbosity"
       <> short "v"
       <> metavar "LEVEL"
       <> help "Log level for the application. Valid values: info|debug|quiet" )
  <.> option (\s a -> readRcOption s >.> \kv ->
                      a { optDefConfig = optDefConfig a ++ [kv] })
        (  long "define"
        <> short "d"
        <> metavar "DEFINITION"
        <> help "Overwrite definition of cpmrc file with 'option=value'." )
  <.> flag (\a -> Right $ a { optWithTime = True })
        (  long "time"
        <> short "t"
        <> help "Show elapsed time with every log output" )
  <.> commands (metavar "COMMAND")
        (   command "config"
                    (help "Show current configuration of CPM")
                    (\a -> Right $ a { optCommand = Config (configOpts a) })
                    configArgs
        <|> command "checkout" (help "Checkout a package.") Right
                    (checkoutArgs Checkout)
        <|> command "install" (help "Install a package with its dependencies.")
                     (\a -> Right $ a { optCommand = Install (installOpts a) })
                     installArgs
        <|> command "uninstall" (help "Uninstall a package")
                 (\a -> Right $ a { optCommand = Uninstall (uninstallOpts a) })
                 uninstallArgs
        <|> command "deps" (help "Calculate dependencies")
                           (\a -> Right $ a { optCommand = Deps (depsOpts a) })
                           depsArgs
        <|> command "clean" (help "Clean the current package")
                            (\a -> Right $ a { optCommand = Clean }) []
        <|> command "new" (help "Create a new package") Right newArgs
        <|> command "update" (help "Update the package index")
                             (\a -> Right $ a { optCommand = Update }) []
        <|> command "curry"
           (help "Load package spec and start Curry with correct dependencies.")
                 (\a -> Right $ a { optCommand = Compiler (execOpts a) })
                 curryArgs
        <|> command "exec"
                    (help "Execute a command with the CURRYPATH set")
                    (\a -> Right $ a { optCommand = Exec (execOpts a) })
                    execArgs
        <|> command "info" (help "Print package information")
                    (\a -> Right $ a { optCommand = PkgInfo (infoOpts a) })
                    infoArgs
        <|> command "doc"
           (help "Generation documentation for current package (with CurryDoc)")
                    (\a -> Right $ a { optCommand = Doc (docOpts a) })
                    docArgs
        <|> command "test" (help "Test the current package (with CurryCheck)")
                    (\a -> Right $ a { optCommand = Test (testOpts a) })
                    testArgs
        <|> command "diff"
                    (help "Diff the current package against another version")
                    (\a -> Right $ a { optCommand = Diff (diffOpts a) })
                    diffArgs
        <|> command "list" (help "List all packages of the repository")
                    (\a -> Right $ a { optCommand = List (listOpts a) })
                    listArgs
        <|> command "search" (help "Search the package repository") Right
                    searchArgs
        <|> command "upgrade" (help "Upgrade one or more packages")
                    (\a -> Right $ a { optCommand = Upgrade (upgradeOpts a) })
                    upgradeArgs
        <|> command "link" (help "Link a package to the local cache") Right
                    linkArgs
        <|> command "add"
              (help "Add a package (as dependency or to the local repository)")
              Right
              addArgs ) )
 where
  configArgs =
    flag (\a -> Right $ a { optCommand = Config (configOpts a)
                                                { configAll = True } })
         (  short "a"
         <> long "all"
         <> help "Show also names of installed packages"
         <> optional )

  depsArgs =
    flag (\a -> Right $ a { optCommand = Deps (depsOpts a)
                                              { depsPath = True } })
         (  short "p"
         <> long "path"
         <> help "Show value of CURRYPATH only"
         <> optional )

  checkoutArgs cmd =
        arg (\s a -> Right $ a { optCommand = cmd (checkoutOpts a)
                                                  { coPackage = s } })
          (  metavar "PACKAGE"
          <> help "The package name" )
    <.> arg (\s a -> readVersion' s >.> \v ->
                     a { optCommand = cmd (checkoutOpts a)
                                          { coVersion = Just v } })
          (  metavar "VERSION"
          <> help "The package version"
          <> optional)
    <.> flag (\a -> Right $ a { optCommand = cmd (checkoutOpts a)
                                                 { coPrerelease = True } })
          (  short "p"
          <> long "pre"
          <> help "Try pre-release versions when searching for newest version.")

  installArgs =
        arg (\s a -> Right $ a { optCommand = Install (installOpts a)
                                                      { instTarget = Just s } })
          (  metavar "TARGET"
          <> help "A package name or the path to a file"
          <> optional)
    <.> arg (\s a -> readVersion' s >.> \v ->
             a { optCommand = Install (installOpts a) { instVersion = Just v } })
          (  metavar "VERSION"
          <> help "The package version"
          <> optional)
    <.> flag (\a -> Right $ a { optCommand = Install (installOpts a)
                                               { instPrerelease = True } })
          (  short "p"
          <> long "pre"
          <> help "Try pre-release versions when searching for newest version.")
    <.> flag (\a -> Right $ a { optCommand = Install (installOpts a)
                                               { instExecutable = False } })
          (  short "n"
          <> long "noexec"
          <> help "Do not install executable.")
    <.> flag (\a -> Right $ a { optCommand = Install (installOpts a)
                                               { instExecOnly = True } })
          (  short "x"
          <> long "exec"
          <> help "Install executable only (do not re-install dependencies).")

  uninstallArgs =
        arg (\s a -> Right $ a { optCommand =
             Uninstall (uninstallOpts a) { uninstPackage = Just s } })
          (  metavar "PACKAGE"
          <> help "The package to be uninstalled"
          <> optional)
    <.> arg (\s a -> readVersion' s >.> \v ->
                     a { optCommand = Uninstall (uninstallOpts a)
                                                { uninstVersion = Just v } })
          (  metavar "VERSION"
          <> help "The version to be uninstalled"
          <> optional)

  newArgs =
   arg (\s a -> Right $ a { optCommand = New (newOpts a)
                                             { projectName = s } })
       (  metavar "PROJECT"
       <> help "The name of the new project" )

  curryArgs =
    rest (\_ a -> Right $ a { optCommand = Compiler (execOpts a)
                                            { exeCommand = unwords remargs } })
         (  metavar "ARGS"
         <> help "The options to pass to the compiler"
         <> optional )
    where
     remargs = tail (snd (break (=="curry") allargs))

  execArgs =
    rest (\_ a -> Right $ a { optCommand = Exec (execOpts a)
                                            { exeCommand = unwords remargs } })
         (  metavar "CMD"
         <> help "The command to be executed."
         <> optional )
    where
     remargs = tail (snd (break (=="exec") allargs))

  infoArgs =
        arg (\s a -> Right $ a { optCommand = PkgInfo (infoOpts a)
                                                { infoPackage = Just s } })
          (  metavar "PACKAGE"
          <> help ("The package name. If no name is specified, CPM tries " ++
                   "to read a package specification in the current directory.")
          <> optional) 
    <.> arg (\s a -> readVersion' s >.> \v -> a
                                 { optCommand = PkgInfo (infoOpts a)
                                                  { infoVersion = Just v } })
          (  metavar "VERSION"
          <> help ("The package version. If no version is specified, " ++
                   "CPM uses the latest version of the specified package.")
          <> optional )
    <.> flag (\a -> Right $ a { optCommand = PkgInfo (infoOpts a)
                                               { infoAll = True } })
          (  short "a"
          <> long "all"
          <> help "Show all infos" )
    <.> flag (\a -> Right $ a { optCommand = PkgInfo (infoOpts a)
                                               { infoPlain = True } })
          (  short "p"
          <> long "plain"
          <> help "Plain output (no control characters for bold or colors)"
          <> optional )

  docArgs =
    option (\s a -> Right $ a { optCommand =
                                  Doc (docOpts a) { docDir = Just $ s } })
          (  long "docdir"
          <> short "d"
          <> help "The documentation directory (default: 'cdoc')"
          <> optional )
    <.>
    option (\s a -> Right $ a { optCommand = Doc (docOpts a)
                                      { docModules = Just $ splitOn "," s } })
          (  long "modules"
          <> short "m"
          <> help ("The modules to be documented, " ++
                   "separate multiple modules by comma")
          <> optional )
    <.> flag (\a -> Right $ a { optCommand = Doc (docOpts a)
                                               { docManual = False } })
          (  short "p"
          <> long "programs"
          <> help "Generate only program documentation (with CurryDoc)"
          <> optional )
    <.> flag (\a -> Right $ a { optCommand = Doc (docOpts a)
                                               { docPrograms = False } })
          (  short "t"
          <> long "text"
          <> help "Generate only manual (according to package specification)"
          <> optional )
    <.> flag (\a -> Right $ a { optCommand = Doc (docOpts a)
                                               { docGenImports = True } })
          (  short "f"
          <> long "full"
          <> help "Generate full program documentation (i.e., also imported packages)"
          <> optional )

  testArgs =
    option (\s a -> Right $ a { optCommand = Test (testOpts a)
                                      { testModules = Just $ splitOn "," s } })
          (  long "modules"
          <> short "m"
          <> help "The modules to be tested, separate multiple modules by comma"
          <> optional )

  diffArgs =
       arg (\s a -> readVersion' s >.> \v ->
                 a { optCommand = Diff (diffOpts a) { diffVersion = Just v } })
           (  metavar "VERSION"
           <> help ("The other package version. If no version is specified, " ++
                    "CPM diffs against the latest repository version.")
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
         <> help
         "Do not use automatic termination analysis for safe behavior checking")

  listArgs =
        flag (\a -> Right $ a { optCommand =
                                  List (listOpts a) { listVers = True } })
          (  short "v"
          <> long "versions"
          <> help "Show all versions" ) 
    <.> flag (\a -> Right $ a { optCommand =
                                  List (listOpts a) { listCSV = True } })
          (  short "t"
          <> long "csv"
          <> help "Show in CSV table format" )
    <.> flag (\a -> Right $ a { optCommand =
                                  List (listOpts a) { listCat = True } })
          (  short "c"
          <> long "category"
          <> help "Show all categories" )

  searchArgs =
        flag (\a -> Right $ a { optCommand = Search (searchOpts a)
                                               { searchModule = True } })
             (  short "m"
             <> long "module"
             <> help "Search for an exported module" )
    <.> flag (\a -> Right $ a { optCommand = Search (searchOpts a)
                                               { searchExec = True } })
             (  short "x"
             <> long "exec"
             <> help "Search for the name of an executable" )
    <.> arg (\s a -> Right $ a { optCommand = Search (searchOpts a)
                                                { searchQuery = s } }) 
            (  metavar "QUERY"
            <> help "The search term" )

  upgradeArgs =
    arg (\s a -> Right $ a { optCommand = Upgrade (upgradeOpts a)
                                            { upgrTarget = Just s } })
        (  metavar "PACKAGE"
        <> help "The package to upgrade" 
        <> optional )

  linkArgs =
    arg (\s a -> Right $ a { optCommand = Link (linkOpts a) { lnkSource = s } })
        (  metavar "SOURCE"
        <> help "The directory to link" )

  addArgs =
       flag (\a -> Right $ a { optCommand =
                                 Add (addOpts a) { addPackage = True } })
            (  short "p"
            <> long "package"
            <> help "Add a local package to the local repository" )
   <.> flag (\a -> Right $ a { optCommand =
                                 Add (addOpts a) { addDependency = True } })
            (  short "d"
            <> long "dependency"
            <> help "Add a dependency to the current package" )
   <.> flag (\a -> Right $ a { optCommand =
                                 Add (addOpts a) { forceAdd = True } })
            (  short "f"
            <> long "force"
            <> help "Force, i.e., overwrite existing package" )
   <.> arg (\s a -> Right $ a { optCommand =
                                  Add (addOpts a) { addSource = s } })
           (  metavar "PACKAGE"
           <> help "The package directory or name to be added" )

-- Check if operating system executables we depend on are present on the
-- current system. Since this takes some time, it is only checked with
-- the `update` command.
checkRequiredExecutables :: IO ()
checkRequiredExecutables = do
  debugMessage "Checking whether all required executables can be found..."
  missingExecutables <- checkExecutables listOfExecutables
  unless (null missingExecutables) $ do
      putStrLn $ "The following programs could not be found on the PATH " ++
                 "(they are required for CPM to work):\n" ++
                 intercalate ", " missingExecutables
      exitWith 1
  debugMessage "All required executables found."
 where
  listOfExecutables = 
    [ "curl"  
    , "git"   
    , "unzip" 
    , "cp"
    , "rm"
    , "ln"
    , "readlink" ]

checkExecutables :: [String] -> IO [String]
checkExecutables executables = do
  present <- mapIO fileInPath executables
  return $ map fst $ filter (not . snd) (zip executables present)

------------------------------------------------------------------------------
-- `config` command: show current CPM configuration
configCmd :: ConfigOptions -> Config -> IO (ErrorLogger ())
configCmd opts cfg = do
  if configAll opts
    then readRepository cfg False >>= \repo ->
         readGlobalCache cfg repo |>= \gc ->
         putStrLn configS >>
         putStrLn "Installed packages:\n" >>
         putStrLn (unwords (map packageId (allPackages gc))) >> succeedIO ()
    else putStrLn configS >> succeedIO ()
 where
  configS = unlines
             [cpmBanner, "Current configuration:", "", showConfiguration cfg]

------------------------------------------------------------------------------
-- `update` command:
updateCmd :: Config -> IO (ErrorLogger ())
updateCmd cfg =
  checkRequiredExecutables >> updateRepository cfg
  
------------------------------------------------------------------------------
-- `deps` command:
depsCmd :: DepsOptions -> Config -> IO (ErrorLogger ())
depsCmd opts cfg =
  getLocalPackageSpec cfg "." |>= \specDir ->
  loadPackageSpec specDir |>= \pkg ->
  checkCompiler cfg pkg >>
  if depsPath opts -- show CURRYPATH only?
    then loadCurryPathFromCache cfg specDir |>=
         maybe (computePackageLoadPath cfg specDir)
               succeedIO |>= \currypath ->
         putStrLn currypath >> succeedIO ()
    else resolveDependencies cfg specDir |>= \result ->
         putStrLn (showResult result) >> succeedIO ()

------------------------------------------------------------------------------
-- `info` command:
infoCmd :: InfoOptions -> Config -> IO (ErrorLogger ())
infoCmd (InfoOptions Nothing (Just _) _ _) _ =
  failIO "Must specify package name"
infoCmd (InfoOptions Nothing Nothing allinfos plain) cfg =
  getLocalPackageSpec cfg "." |>= \specDir ->
  loadPackageSpec specDir |>= \p -> 
  printInfo cfg allinfos plain p
infoCmd (InfoOptions (Just pkgname) Nothing allinfos plain) cfg =
  getAllPackageVersions cfg pkgname False >>= \pkgs ->
  case pkgs of
    [] -> packageNotFoundFailure pkgname
    ps -> case filter (isCompatibleToCompiler cfg) ps of
           [] -> let lvers = showVersion (version (head ps))
                 in compatPackageNotFoundFailure cfg pkgname
                      ("Use 'info " ++ pkgname ++ " " ++ lvers ++
                       "' to print info about the latest version.")
           (rp:_) -> readPackageFromRepository cfg rp |>= \p ->
                     printInfo cfg allinfos plain p
infoCmd (InfoOptions (Just pkgname) (Just v) allinfos plain) cfg =
  getPackageVersion cfg pkgname v >>= \mbpkg ->
  case mbpkg of
    Nothing -> packageNotFoundFailure $ pkgname ++ "-" ++ showVersion v
    Just rp -> readPackageFromRepository cfg rp |>=  \p ->
               printInfo cfg allinfos plain p

printInfo :: Config -> Bool -> Bool -> Package -> IO (ErrorLogger ())
printInfo cfg allinfos plain pkg =
  packageInstalled cfg pkg >>= \isinstalled ->
  putStrLn (renderPackageInfo allinfos plain isinstalled pkg) >> succeedIO ()


------------------------------------------------------------------------------
-- `checkout` command:
checkoutCmd :: CheckoutOptions -> Config -> Repository -> IO (ErrorLogger ())
checkoutCmd (CheckoutOptions pkgname Nothing pre) cfg repo =
 case findAllVersions repo pkgname pre of
  [] -> packageNotFoundFailure pkgname
  ps -> case filter (isCompatibleToCompiler cfg) ps of
    []    -> compatPackageNotFoundFailure cfg pkgname useUpdateHelp
    (p:_) -> acquireAndInstallPackageWithDependencies cfg repo p |>
             checkoutPackage cfg p
checkoutCmd (CheckoutOptions pkg (Just ver) _) cfg repo =
 case findVersion repo pkg ver of
  Nothing -> packageNotFoundFailure $ pkg ++ "-" ++ showVersion ver
  Just  p -> acquireAndInstallPackageWithDependencies cfg repo p |>
             checkoutPackage cfg p

installCmd :: InstallOptions -> Config -> Repository -> IO (ErrorLogger ())
installCmd (InstallOptions Nothing Nothing _ instexec False) cfg repo =
  getLocalPackageSpec cfg "." |>= \pkgdir ->
  cleanCurryPathCache pkgdir |>
  installLocalDependencies cfg repo pkgdir |>= \ (pkg,_) ->
  saveBaseVersionToCache cfg pkgdir >>
  writePackageConfig cfg pkgdir pkg |>
  if instexec then installExecutable cfg pkg else succeedIO ()
-- Install executable only:
installCmd (InstallOptions Nothing Nothing _ _ True) cfg _ =
  getLocalPackageSpec cfg "." |>= \pkgdir ->
  loadPackageSpec pkgdir |>= \pkg ->
  installExecutable cfg pkg
installCmd (InstallOptions (Just pkg) vers pre _ _) cfg repo = do
  fileExists <- doesFileExist pkg
  if fileExists
    then installFromZip cfg pkg
    else installapp (CheckoutOptions pkg vers pre) cfg repo
installCmd (InstallOptions Nothing (Just _) _ _ _) _ _ =
  failIO "Must specify package name"

--- Installs the application (i.e., binary) provided by a package.
--- This is done by checking out the package into CPM's application packages
--- cache (default: $HOME/.cpm/app_packages, see APP_PACKAGE_PATH
--- in .cpmrc configuration file) and then install this package.
---
--- Internal note: the installed package should not be cleaned or removed
--- after the installation since its execution might refer (via the
--- config module) to some data stored in the package.
installapp :: CheckoutOptions -> Config -> Repository -> IO (ErrorLogger ())
installapp opts cfg repo = do
  let apppkgdir = appPackageDir cfg
      copkgdir  = apppkgdir </> coPackage opts
  curdir <- getCurrentDirectory
  removeDirectoryComplete copkgdir
  debugMessage ("Change into directory " ++ apppkgdir)
  inDirectory apppkgdir
    ( checkoutCmd opts cfg repo |>
      log Debug ("Change into directory " ++ copkgdir) |>
      (setCurrentDirectory copkgdir >> succeedIO ()) |>
      loadPackageSpec "." |>= \pkg ->
      maybe (setCurrentDirectory curdir >>
             removeDirectoryComplete copkgdir >>
             failIO ("Package '" ++ name pkg ++
                     "' does not contain an executable, nothing installed."))
            (\_ -> installCmd (InstallOptions Nothing Nothing False True False)
                              cfg repo)
            (executableSpec pkg)
    )

--- Checks the compiler compatibility.
checkCompiler :: Config -> Package -> IO ()
checkCompiler cfg pkg =
  unless (isCompatibleToCompiler cfg pkg)
    (error $ "Current compiler '" ++ showCompilerVersion cfg ++
             "' incompatible to package specification!")

--- Installs the executable specified in the package in the
--- bin directory of CPM (compare .cpmrc).
installExecutable :: Config -> Package -> IO (ErrorLogger ())
installExecutable cfg pkg =
  checkCompiler cfg pkg >>
  maybe (succeedIO ())
        (\ (PackageExecutable name mainmod eopts) ->
           getLogLevel >>= \lvl ->
           getEnviron "PATH" >>= \path ->
           log Info ("Compiling main module: " ++ mainmod) |>
           let (cmpname,_,_) = compilerVersion cfg
               cmd = unwords $
                       [":set", if levelGte Debug lvl then "v1" else "v0"
                       , maybe "" id (lookup cmpname eopts)
                       , ":load", mainmod, ":save", ":quit"]
               bindir     = binInstallDir cfg
               binexec    = bindir </> name
           in compiler (ExecOptions cmd) cfg |>
              log Info ("Installing executable '" ++ name ++ "' into '" ++
                        bindir ++ "'") |>
              (whenFileExists binexec (backupExistingBin binexec) >>
               -- renaming might not work across file systems, hence we move:
               showExecCmd (unwords ["mv", mainmod, binexec]) >>
               checkPath path bindir))
        (executableSpec pkg)
 where
  backupExistingBin binexec = do
    let binexecbak = binexec ++ ".bak"
    showExecCmd $ "rm -f " ++ binexecbak
    renameFile binexec binexecbak
    infoMessage $ "Existing executable '" ++ binexec ++ "' saved to '" ++
                  binexecbak ++ "'."

  checkPath path bindir =
    if bindir `elem` splitSearchPath path
      then succeedIO ()
      else log Info $ "It is recommended to add '" ++bindir++ "' to your path!"


uninstall :: UninstallOptions -> Config -> IO (ErrorLogger ())
uninstall (UninstallOptions (Just pkgname) (Just ver)) cfg =
  uninstallPackage cfg pkgname ver
--- uninstalls an application (i.e., binary) provided by a package:
uninstall (UninstallOptions (Just pkgname) Nothing) cfg = do
  let copkgdir  = appPackageDir cfg </> pkgname
  codirexists <- doesDirectoryExist copkgdir
  if codirexists
    then loadPackageSpec copkgdir |>= uninstallPackageExecutable cfg |>
         removeDirectoryComplete copkgdir >>
         log Info ("Package '" ++ pkgname ++
                   "' uninstalled from application package cache.")
    else failIO $ "Package '" ++ pkgname ++ "' is not installed."
uninstall (UninstallOptions Nothing (Just _)) _ =
  log Error "Please provide a package and version number!"
uninstall (UninstallOptions Nothing Nothing) cfg =
  getLocalPackageSpec cfg "." |>= \pkgdir ->
  loadPackageSpec pkgdir |>= uninstallPackageExecutable cfg

uninstallPackageExecutable :: Config -> Package -> IO (ErrorLogger ())
uninstallPackageExecutable cfg pkg =
  maybe (succeedIO ())
        (\ (PackageExecutable name _ _) ->
           let binexec = binInstallDir cfg </> name
           in ifFileExists binexec
                (removeFile binexec >>
                 log Info ("Executable '" ++ binexec ++ "' removed"))
                (log Info $ "Executable '" ++ binexec ++ "' not installed"))
        (executableSpec pkg)

tryFindVersion :: String -> Version -> Repository -> IO (ErrorLogger Package)
tryFindVersion pkg ver repo = case findVersion repo pkg ver of
  Nothing -> packageNotFoundFailure $ pkg ++ "-" ++ showVersion ver
  Just  p -> succeedIO $ p

--- Lists all (compiler-compatible if `lall` is false) packages
--- in the given repository.
listCmd :: ListOptions -> Config -> Repository -> IO (ErrorLogger ())
listCmd (ListOptions lv csv cat) cfg repo =
  let listresult = if cat then renderCats catgroups
                          else renderPkgs allpkgs
  in putStr listresult >> succeedIO ()
 where
  -- all packages (and versions if `lv`)
  allpkgs = concatMap (if lv then id else ((:[]) . filterCompatPkgs cfg))
                      (sortBy (\ps1 ps2 -> name (head ps1) <= name (head ps2))
                              (listPackages repo))

  -- all categories together with their package names:
  catgroups =
    let pkgid p = name p ++ '-' : showVersionIfCompatible cfg p
        newpkgs = map (filterCompatPkgs cfg) (listPackages repo)
        catpkgs = concatMap (\p -> map (\c -> (c, pkgid p)) (category p))
                            newpkgs
        nocatps = map pkgid (filter (null . category) newpkgs)
    in map (\cg -> (fst (head cg), map snd cg))
           (groupBy (\ (c1,_) (c2,_) -> c1==c2) (nub $ sortBy (<=) catpkgs)) ++
       if null nocatps then []
                       else [("???", nub $ sortBy (<=) nocatps)]

  renderPkgs pkgs =
    let (colsizes,rows) = packageVersionAsTable cfg pkgs
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
packageVersionAsTable :: Config -> [Package] -> ([Int],[[String]])
packageVersionAsTable cfg pkgs = (colsizes, rows)
 where
  namelen = foldl max 4 $ map (length . name) pkgs
  colsizes = [namelen + 2, 68 - namelen, 10]
  header  = [ ["Name", "Synopsis", "Version"]
            , ["----", "--------", "-------"]]
  rows    = header ++ map formatPkg pkgs
  formatPkg p = [name p, synopsis p, showVersionIfCompatible cfg p]

--- Shows the version of a package if it is compatible with the
--- current compiler, otherwise shows the version in brackets.
showVersionIfCompatible :: Config -> Package -> String
showVersionIfCompatible cfg p =
  let s = showVersion (version p)
  in if isCompatibleToCompiler cfg p then s else '(' : s ++ ")"

cpmInfo :: String
cpmInfo = "Use 'cypm info PACKAGE' for more information about a package."


--- Search in all (compiler-compatible) packages in the given repository.
searchCmd :: SearchOptions -> Config -> Repository -> IO (ErrorLogger ())
searchCmd (SearchOptions q smod sexec) cfg repo =
  putStr rendered >> succeedIO ()
 where
  results = sortBy (\p1 p2 -> name p1 <= name p2)
                   (map (filterCompatPkgs cfg)
                        (searchPackages repo smod sexec q))
  (colsizes,rows) = packageVersionAsTable cfg results
  rendered = unlines $
               if null results
                 then [ "No packages found for '" ++ q ++ "'", useUpdateHelp ]
                 else [ render (table rows colsizes), cpmInfo, useUpdateHelp ]


--- `upgrade` command.
upgradeCmd :: UpgradeOptions -> Config -> Repository -> IO (ErrorLogger ())
upgradeCmd (UpgradeOptions Nothing) cfg repo =
  getLocalPackageSpec cfg "." |>= \specDir ->
  cleanCurryPathCache specDir |>
  log Info "Upgrading all packages" |>
  upgradeAllPackages cfg repo specDir
upgradeCmd (UpgradeOptions (Just pkg)) cfg repo =
  getLocalPackageSpec cfg "." |>= \specDir ->
  log Info ("Upgrade " ++ pkg) |>
  upgradeSinglePackage cfg repo specDir pkg


--- `link` command.
linkCmd :: LinkOptions -> Config -> IO (ErrorLogger ())
linkCmd (LinkOptions src) cfg =
  getLocalPackageSpec cfg "." |>= \specDir ->
  cleanCurryPathCache specDir |>
  log Info ("Linking '" ++ src ++ "' into local package cache...") |>
  linkToLocalCache src specDir

--- `add` command: copy the given package to the repository index
--- and package installation directory so that it is available as
--- any other package.
addCmd :: AddOptions -> Config -> IO (ErrorLogger ())
addCmd (AddOptions addpkg adddep pkg force) config
  | addpkg    = addPackageCmd    pkg force config
  | adddep    = addDependencyCmd pkg force config
  | otherwise = log Critical "Option --package or --dependency missing!"

--- `add --package` command: copy the given package to the repository index
--- and package installation directory so that it is available as
--- any other package.
addPackageCmd :: String -> Bool -> Config -> IO (ErrorLogger ())
addPackageCmd pkgdir force config = do
  dirExists <- doesDirectoryExist pkgdir
  if dirExists
    then loadPackageSpec pkgdir |>= \pkgSpec ->
         (copyPackage pkgSpec >> succeedIO ()) |>
         log Info ("Package in directory '" ++ pkgdir ++
                   "' installed into local repository")
    else log Critical ("Directory '" ++ pkgdir ++ "' does not exist.") |>
         succeedIO ()
 where
  copyPackage pkg = do
    let pkgName          = name pkg
        pkgVersion       = version pkg
        pkgIndexDir      = pkgName </> showVersion pkgVersion
        pkgRepositoryDir = repositoryDir config </> pkgIndexDir
        pkgInstallDir    = packageInstallDir config </> packageId pkg
    exrepodir <- doesDirectoryExist pkgRepositoryDir
    when (exrepodir && not force) $ error $
      "Package repository directory '" ++
      pkgRepositoryDir ++ "' already exists!\n" ++ useForce
    expkgdir <- doesDirectoryExist pkgInstallDir
    when expkgdir $
      if force then removeDirectoryComplete pkgInstallDir
               else error $ "Package installation directory '" ++
                            pkgInstallDir ++ "' already exists!\n" ++ useForce
    infoMessage $ "Create directory: " ++ pkgRepositoryDir
    createDirectoryIfMissing True pkgRepositoryDir
    copyFile (pkgdir </> "package.json") (pkgRepositoryDir </> "package.json")
    copyDirectory pkgdir pkgInstallDir
    cleanRepositoryCache config

useForce :: String
useForce = "Use option '-f' or '--force' to overwrite it."

--- `add --dependency` command: add the given package as a new
--- dependency to the current package.
addDependencyCmd :: String -> Bool -> Config -> IO (ErrorLogger ())
addDependencyCmd pkgname force config =
  getAllPackageVersions config pkgname False >>= \allpkgs ->
  case allpkgs of
    [] -> packageNotFoundFailure pkgname
    ps -> case filter (isCompatibleToCompiler config) ps of
             []    -> compatPackageNotFoundFailure config pkgname useUpdateHelp
             (p:_) -> getLocalPackageSpec config "." |>=
                      addDepToLocalPackage (version p)
 where
  addDepToLocalPackage vers pkgdir =
    loadPackageSpec pkgdir |>= \pkgSpec ->
    let depexists = pkgname `elem` dependencyNames pkgSpec
        newdeps   = addDep [[VGte vers]] (dependencies pkgSpec)
        newpkg    = pkgSpec { dependencies = newdeps }
    in if force || not depexists
         then writePackageSpec newpkg (pkgdir </> "package.json") |>>
              log Info ("Dependency '" ++ pkgname ++ " >= " ++
                        showVersion vers ++
                        "' added to package '" ++ pkgdir ++ "'")
         else log Critical ("Dependency '" ++ pkgname ++
                            "' already exists!\n" ++ useForce)

  addDep vcs [] = [Dependency pkgname vcs]
  addDep vcs (Dependency pn pvcs : deps) =
    if pn == pkgname then Dependency pn vcs : deps
                     else Dependency pn pvcs : addDep vcs deps

------------------------------------------------------------------------------
--- `doc` command: run `curry doc` on the modules provided as an argument
--- or, if they are not given, on exported modules (if specified in the
--- package), on the main executable (if specified in the package),
--- or on all source modules of the package.
docCmd :: DocOptions -> Config -> IO (ErrorLogger ())
docCmd opts cfg =
  getLocalPackageSpec cfg "." |>= \specDir ->
  loadPackageSpec specDir |>= \pkg -> do
  let docdir = maybe "cdoc" id (docDir opts) </> packageId pkg
  absdocdir <- getAbsolutePath docdir
  createDirectoryIfMissing True absdocdir
  (if docManual opts then genPackageManual opts cfg pkg absdocdir
                     else succeedIO ()) |>
    (if docPrograms opts then genDocForPrograms opts cfg absdocdir specDir pkg
                         else succeedIO ())

--- Generate manual according to  documentation specification of package.
genPackageManual :: DocOptions -> Config -> Package -> String
                 -> IO (ErrorLogger ())
genPackageManual _ _ pkg outputdir = case documentation pkg of
    Nothing -> succeedIO ()
    Just (PackageDocumentation docdir docmain doccmd) -> do
      let formatcmd = replaceSubString "OUTDIR" outputdir $
                        if null doccmd then formatCmd docmain
                                       else doccmd
      if null formatcmd
        then infoMessage $ "Cannot format documentation file '" ++
                           docmain ++ "' (unknown kind)"
        else do
          debugMessage $ "Executing command: " ++ formatcmd
          inDirectory docdir $ system formatcmd
          let outfile = outputdir </> replaceExtension docmain ".pdf"
          system ("chmod -f 644 " ++ quote outfile) -- make it readable
          infoMessage $ "Package documentation written to '" ++ outfile ++ "'."
      succeedIO ()
 where
  formatCmd docmain
    | ".tex" `isSuffixOf` docmain
    = let formatcmd = "pdflatex -output-directory=\"OUTDIR\" " ++ docmain
      in formatcmd ++ " && " ++ formatcmd
    | ".md" `isSuffixOf` docmain
    = "pandoc " ++ docmain ++
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
--- run `curry doc` on the modules provided as an argument
--- or, if they are not given, on exported modules (if specified in the
--- package), on the main executable (if specified in the package),
--- or on all source modules of the package.
genDocForPrograms :: DocOptions -> Config -> String -> String -> Package
                  -> IO (ErrorLogger ())
genDocForPrograms opts cfg docdir specDir pkg = do
  abspkgdir <- getAbsolutePath specDir
  checkCompiler cfg pkg
  let exports = exportedModules pkg
      mainmod = maybe Nothing
                      (\ (PackageExecutable _ emain _) -> Just emain)
                      (executableSpec pkg)
  (docmods,apidoc) <-
     maybe (if null exports
              then maybe (curryModulesInDir (specDir </> "src") >>=
                          \ms -> return (ms,True))
                         (\m -> return ([m],False))
                         mainmod
              else return (exports,True))
           (\ms -> return (ms,True))
           (docModules opts)
  if null docmods
    then putStrLn "No modules to be documented!" >> succeedIO ()
    else
      loadCurryPathFromCache cfg specDir |>=
      maybe (computePackageLoadPath cfg specDir)
            succeedIO |>= \currypath ->
      let pkgurls = path2packages abspkgdir currypath in
      if apidoc
        then foldEL (\_ -> docModule currypath pkgurls) () docmods |>
             runDocCmd currypath pkgurls
                       (["--title", apititle, "--onlyindexhtml", docdir]
                        ++ docmods) |>
             log Info ("Documentation generated in '"++docdir++"'")
        else runDocCmd currypath pkgurls [docdir, head docmods]
 where
  apititle = "\"API Documentation of Package '" ++ name pkg ++ "'\""

  currydoc = curryExec cfg ++ " doc"

  docModule currypath uses mod =
    runDocCmd currypath uses ["--noindexhtml", docdir, mod]

  runDocCmd currypath uses docparams = do
    let useopts = if docGenImports opts
                    then []
                    else map (\ (d,u) -> "--use "++d++"@"++u) uses
        cmd = unwords (currydoc : useopts ++ docparams)
    infoMessage $ "Running CurryDoc: " ++ cmd
    execWithCurryPath (ExecOptions cmd) cfg currypath

  -- translates a path into a list of package paths and their doc URLs:
  path2packages absdir path =
    let dirs = splitSearchPath path
        importPkgDir = absdir </> ".cpm" </> "packages" ++ [pathSeparator]
        isImportedPackage d = importPkgDir `isPrefixOf` d
        impPkg2URL p =
          let (d,_) = break isPathSeparator (drop (length importPkgDir) p)
          in baseDocURL ++ "/" ++ d
    in map (\ip -> (ip,impPkg2URL ip)) (filter isImportedPackage dirs) ++
       if name pkg == "base"
         then [] -- in order to generate base package documentation
         else [(installDir </> "lib",
                baseDocURL ++ "/base-" ++ compilerBaseVersion cfg)]

-- The URL prefix where all repository packages are documented.
baseDocURL :: String
baseDocURL = "http://www.informatik.uni-kiel.de/~mh/curry/cpm/DOC"

------------------------------------------------------------------------------
--- `test` command: run `curry check` on the modules provided as an argument
--- or, if they are not provided, on the exported (if specified)
--- or all source modules of the package.
testCmd :: TestOptions -> Config -> IO (ErrorLogger ())
testCmd opts cfg =
  getLocalPackageSpec cfg "." |>= \specDir ->
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
    showExecCmd (unwords ["rm", "-rf", currysubdir])
    inDirectory (apkgdir </> dir) $
      execWithPkgDir (ExecOptions testcmd) cfg apkgdir

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

--- Get the names of all Curry modules contained in a directory.
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


diffCmd :: DiffOptions -> Config -> Repository -> IO (ErrorLogger ())
diffCmd opts cfg repo =
  readGlobalCache cfg repo |>= \gc ->
  getLocalPackageSpec cfg "." |>= \specDir ->
  loadPackageSpec specDir     |>= \localSpec ->
  checkCompiler cfg localSpec >>
  let localname  = name localSpec
      localv     = version localSpec
      showlocalv = showVersion localv
  in
  getDiffVersion localname |>= \diffv ->
  if diffv == localv
    then failIO $ "Cannot diff identical package versions " ++ showlocalv
    else putStrLn ("Comparing local version " ++ showlocalv ++
                   " and repository version " ++ showVersion diffv ++ ":\n") >>
         diffAPIIfEnabled      gc specDir localSpec diffv |> 
         diffBehaviorIfEnabled gc specDir localSpec diffv
 where
  getDiffVersion localname = case diffVersion opts of
    Nothing -> case findLatestVersion cfg repo localname False of
      Nothing -> failIO $
        "No other version of local package '" ++ localname ++
        "' compatible to '" ++ showCompilerVersion cfg ++
        "' found in package repository."
      Just p  -> succeedIO (version p)
    Just v  -> succeedIO v
 
  diffAPIIfEnabled gc specDir localSpec diffversion =
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

  diffBehaviorIfEnabled gc specDir localSpec diffversion =
    if diffBehavior opts
      then (putStrLn "Preparing behavior diff...\n" >> succeedIO ()) |>
           BDiff.preparePackageAndDir cfg repo gc specDir (name localSpec)
                                                          diffversion |>=
           \i -> BDiff.diffBehavior cfg repo gc i (diffUseAna opts)
                                    (diffModules opts)
      else succeedIO ()

-- Implementation of the "curry" command.
compiler :: ExecOptions -> Config -> IO (ErrorLogger ())
compiler o cfg =
  getLocalPackageSpec cfg "." |>= \pkgdir ->
  loadPackageSpec pkgdir |>= \pkg ->
  checkCompiler cfg pkg >>
  execWithPkgDir
    (ExecOptions $ unwords [curryExec cfg, "--nocypm", exeCommand o])
    cfg pkgdir

execCmd :: ExecOptions -> Config -> IO (ErrorLogger ())
execCmd o cfg =
  getLocalPackageSpec cfg "." |>= execWithPkgDir o cfg

execWithPkgDir :: ExecOptions -> Config -> String -> IO (ErrorLogger ())
execWithPkgDir o cfg specDir =
  loadCurryPathFromCache cfg specDir |>=
  maybe (computePackageLoadPath cfg specDir)
        succeedIO |>= execWithCurryPath o cfg

execWithCurryPath :: ExecOptions -> Config -> String -> IO (ErrorLogger ())
execWithCurryPath o _ currypath =
  log Debug ("Setting CURRYPATH to " ++ currypath) |>
  do setEnviron "CURRYPATH" currypath
     ecode <- showExecCmd (exeCommand o)
     unsetEnviron "CURRYPATH"
     unless (ecode==0) (exitWith ecode)
     succeedIO ()

computePackageLoadPath :: Config -> String -> IO (ErrorLogger String)
computePackageLoadPath cfg pkgdir =
  debugMessage "Computing load path for package..." >>
  loadPackageSpec pkgdir |>= \pkg ->
  loadBaseVersionFromCache pkgdir >>= \cbv ->
  let cfg' = if null cbv then cfg else cfg { baseVersion = cbv } in
  resolveAndCopyDependenciesForPackage cfg' pkgdir pkg |>= \allpkgs ->
  getAbsolutePath pkgdir >>= \abs -> succeedIO () |>
  let srcdirs = map (abs </>) (sourceDirsOf pkg)
      -- remove 'base' package if it is the same as in current config:
      pkgs = filter notCurrentBase allpkgs
      currypath = joinSearchPath (srcdirs ++ dependencyPathsSeparate pkgs abs)
  in saveCurryPathToCache cfg' pkgdir currypath >> succeedIO currypath
 where
  notCurrentBase pkg = name pkg /= "base" ||
                       showVersion (version pkg) /= compilerBaseVersion cfg

-- Clean auxiliary files in the current package
cleanPackage :: Config -> LogLevel -> IO (ErrorLogger ())
cleanPackage cfg ll =
  getLocalPackageSpec cfg "." |>= \specDir ->
  loadPackageSpec specDir     |>= \pkg ->
  let dotcpm   = specDir </> ".cpm"
      srcdirs  = map (specDir </>) (sourceDirsOf pkg)
      testdirs = map (specDir </>)
                     (maybe []
                            (map (\ (PackageTest m _ _ _) -> m))
                            (testSuite pkg))
      rmdirs   = nub (dotcpm : map addCurrySubdir (srcdirs ++ testdirs))
  in log ll ("Removing directories: " ++ unwords rmdirs) |>
     (showExecCmd (unwords $ ["rm", "-rf"] ++ rmdirs) >> succeedIO ())


--- Creates a new package.
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
    , "Then run 'cypm install' to install all dependencies and"
    , "put your program code in directory 'src'"
    , "(where you find a template file 'Main.curry')"
    , ""
    , "Run the main program with:"
    , "> cypm curry :load Main :eval main :quit"
    ]


--- Fail with a "package not found" message.
packageNotFoundFailure :: String -> IO (ErrorLogger _)
packageNotFoundFailure pkgname =
  failIO $ "Package '" ++ pkgname ++ "' not found in package repository.\n" ++
           useUpdateHelp

--- Fail with a "compatible package not found" message and a comment
compatPackageNotFoundFailure :: Config -> String -> String -> IO (ErrorLogger _)
compatPackageNotFoundFailure cfg pkgname helpcmt =
  failIO $ "No version of package '" ++ pkgname ++ "' compatible to '" ++
           showCompilerVersion cfg ++ "' found!\n" ++
           helpcmt

---------------------------------------------------------------------------
-- Caching the current CURRYPATH of a package for faster startup.
-- The file `.cpm/CURRYPATH_CACHE` contains the following lines:
-- * The CURRYPATH used to load the package
-- * The compiler name and major/minor version
-- * The version of the base libraries required during package install

--- The name of the cache file in a package directory.
curryPathCacheFile :: String -> String
curryPathCacheFile pkgdir = pkgdir </> ".cpm" </> "CURRYPATH_CACHE"

--- The name of the cache file for the base version in a package directory.
baseVersionCacheFile :: String -> String
baseVersionCacheFile pkgdir = pkgdir </> ".cpm" </> "BASEVERSION_CACHE"

--- Saves baseVersion of config in local cache file in the given package dir.
saveBaseVersionToCache :: Config -> String -> IO ()
saveBaseVersionToCache cfg pkgdir = do
  let cpmdir = pkgdir </> ".cpm"
  createDirectoryIfMissing False cpmdir
  writeFile (baseVersionCacheFile pkgdir) (baseVersion cfg ++ "\n")

--- Loads baseVersion from local cache file in the given package dir.
loadBaseVersionFromCache :: String -> IO String
loadBaseVersionFromCache pkgdir = do
  let cachefile = baseVersionCacheFile pkgdir
  excache <- doesFileExist cachefile
  if excache
    then do bv <- safeReadFile cachefile >>= return . either (const "") id
            debugMessage $ "Base version loaded from cache: " ++ bv
            return bv
    else return ""

--- Saves package CURRYPATH in local cache file in the given package dir.
saveCurryPathToCache :: Config -> String -> String -> IO ()
saveCurryPathToCache cfg pkgdir path = do
  let cpmdir = pkgdir </> ".cpm"
  createDirectoryIfMissing False cpmdir
  writeFile (curryPathCacheFile pkgdir)
            (unlines [path, showCompilerVersion cfg, baseVersion cfg])

--- Restores package CURRYPATH from local cache file in the given package dir,
--- if it is still up-to-date, i.e., it exists and is newer than the package
--- specification.
loadCurryPathFromCache :: Config -> String -> IO (ErrorLogger (Maybe String))
loadCurryPathFromCache cfg pkgdir = do
  let cachefile = curryPathCacheFile pkgdir
  excache <- doesFileExist cachefile
  if excache
    then do
      cftime <- getModificationTime cachefile
      pftime <- getModificationTime (pkgdir </> "package.json")
      if cftime > pftime
        then do cnt <- safeReadFile cachefile
                let ls = either (const []) lines cnt
                succeedIO $ if consistentCache ls then Just (head ls)
                                                  else Nothing
        else succeedIO Nothing
    else succeedIO Nothing
 where
  consistentCache cls =
    length cls > 2 && cls!!1 == showCompilerVersion cfg
                   && cls!!2 == baseVersion cfg

--- Cleans the local cache file for CURRYPATH. This might be necessary
--- for upgrade/install/link commands.
cleanCurryPathCache :: String -> IO (ErrorLogger ())
cleanCurryPathCache pkgdir = do
  let pathcachefile = curryPathCacheFile pkgdir
      basecachefile = baseVersionCacheFile pkgdir
  whenFileExists pathcachefile $ removeFile pathcachefile
  whenFileExists basecachefile $ removeFile basecachefile
  succeedIO ()

---------------------------------------------------------------------------
