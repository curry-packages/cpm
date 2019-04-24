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
import Distribution ( installDir )
import Either
import FilePath     ( (</>), splitSearchPath, replaceExtension, takeExtension
                    , pathSeparator, isPathSeparator )
import IO           ( hFlush, stdout )
import IOExts       ( evalCmd )
import List         ( groupBy, intercalate, isPrefixOf, isSuffixOf, nub, split
                    , splitOn )
import Sort         ( sortBy )
import System       ( getArgs, getEnviron, setEnviron, unsetEnviron, exitWith
                    , system )

import Boxes            ( table, render )
import OptParse
import System.CurryPath ( addCurrySubdir, stripCurrySuffix )
import System.Path      ( fileInPath )
import Text.CSV         ( showCSV )

import CPM.ErrorLogger
import CPM.FileUtil ( joinSearchPath, safeReadFile, whenFileExists
                    , ifFileExists, inDirectory, inTempDir, recreateDirectory
                    , removeDirectoryComplete, copyDirectory, quote, tempDir )
import CPM.Config   ( Config (..)
                    , readConfigurationWith, showCompilerVersion
                    , showConfiguration )
import CPM.PackageCache.Global ( GlobalCache, readGlobalCache, allPackages
                               , installFromZip, checkoutPackage
                               , uninstallPackage, packageInstalled )
import CPM.Package
import CPM.Package.Helpers ( cleanPackage, getLocalPackageSpec
                           , renderPackageInfo, installPackageSourceTo )
import CPM.Resolution ( isCompatibleToCompiler, showResult )
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
import CPM.ConfigPackage (packagePath)

-- Banner of this tool:
cpmBanner :: String
cpmBanner = unlines [bannerLine,bannerText,bannerLine]
 where
 bannerText =
  "Curry Package Manager <curry-language.org/tools/cpm> (version of 24/04/2019)"
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
    Left err -> do errorMessage $ "Error reading .cpmrc settings: " ++ err
                   exitWith 1
    Right c' -> return c'
  debugMessage ("Current configuration:\n" ++ showConfiguration config)
  (msgs, result) <- case optCommand opts of
    NoCommand   -> failIO "NoCommand"
    Config o    -> configCmd   o config
    Update o    -> updateCmd   o config
    Compiler o  -> compiler    o config
    Exec o      -> execCmd     o config
    Doc  o      -> docCmd      o config
    Test o      -> testCmd     o config
    Uninstall o -> uninstall   o config
    Deps o      -> depsCmd     o config
    PkgInfo   o -> infoCmd     o config
    Link o      -> linkCmd     o config
    Add  o      -> addCmd      o config
    New o       -> newPackage  o 
    List      o -> listCmd     o config
    Search    o -> searchCmd   o config
    Upgrade   o -> upgradeCmd  o config
    Diff      o -> diffCmd     o config
    Checkout  o -> checkoutCmd o config
    Install   o -> installCmd  o config
    Upload    o -> uploadCmd   o config
    Clean       -> cleanPackage config Info
  mapIO showLogEntry msgs
  let allOk =  all (levelGte Info) (map logLevelOf msgs) &&
               either (\le -> levelGte Info (logLevelOf le))
                      (const True)
                      result
  exitWith (if allOk then 0 else 1)

data Options = Options
  { optLogLevel  :: LogLevel
  , optDefConfig :: [(String,String)]
  , optWithTime  :: Bool
  , optCommand   :: Command }

-- The default options: no command, no timing, info log level
defaultOptions :: Options
defaultOptions = Options Info [] False NoCommand

data Command 
  = NoCommand
  | Config     ConfigOptions
  | Deps       DepsOptions
  | Checkout   CheckoutOptions
  | Install    InstallOptions
  | Uninstall  UninstallOptions
  | PkgInfo    InfoOptions
  | Compiler   ExecOptions
  | Update     UpdateOptions
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
  | Upload     UploadOptions

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

data UpdateOptions = UpdateOptions
  { indexURLs :: [String]   -- the URLs of additional index repositories
  , cleanCache :: Bool      -- clean also repository cache?
  }

data UploadOptions = UploadOptions
  { setTag      :: Bool  -- set the tag in the current repository?
  , forceUpdate :: Bool  -- force update if package with same version exists
  }

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
  , docPackageURL :: String          -- the URL prefix where all repository
                                     -- packages are documented
}

data TestOptions = TestOptions
  { testModules :: Maybe [String] }

data DiffOptions = DiffOptions
  { diffVersion   :: Maybe Version   -- version to be compared
  , diffModules   :: Maybe [String]  -- modules to be compared
  , diffAPI       :: Bool            -- check API equivalence
  , diffBehavior  :: Bool            -- test behavior equivalence
  , diffGroundEqu :: Bool            -- test ground equivalence only
  , diffUseAna    :: Bool            -- use termination analysis for safe tests
  }

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

updateOpts :: Options -> UpdateOptions
updateOpts s = case optCommand s of
  Update opts -> opts
  _           -> UpdateOptions [] True

uploadOpts :: Options -> UploadOptions
uploadOpts s = case optCommand s of
  Upload opts -> opts
  _           -> UploadOptions True False

execOpts :: Options -> ExecOptions
execOpts s = case optCommand s of
  Exec opts -> opts
  _         -> ExecOptions ""

docOpts :: Options -> DocOptions
docOpts s = case optCommand s of
  Doc opts -> opts
  _        -> DocOptions Nothing Nothing True True False defaultBaseDocURL

-- The default URL prefix where all repository packages are documented.
-- Can be overwritten with a doc command option.
defaultBaseDocURL :: String
defaultBaseDocURL = "https://www.informatik.uni-kiel.de/~curry/cpm/DOC"


testOpts :: Options -> TestOptions
testOpts s = case optCommand s of
  Test opts -> opts
  _         -> TestOptions Nothing

diffOpts :: Options -> DiffOptions
diffOpts s = case optCommand s of
  Diff opts -> opts
  _         -> DiffOptions Nothing Nothing True True False True

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
applyParse fs = applyEither fs defaultOptions

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
        <|> command "update"
                    (help "Update the package index")
                    (\a -> Right $ a { optCommand = Update (updateOpts a) })
                    updateArgs
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
              addArgs
        <|> command "upload"
                    (help "Upload current package to package server")
                    (\a -> Right $ a { optCommand = Upload (uploadOpts a) })
                    uploadArgs
        ) )
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

  updateArgs =
    option (\s a -> let opts = updateOpts a
                    in Right $ a { optCommand = Update opts
                                          { indexURLs = s : indexURLs opts } })
         (  short "u"
         <> long "url"
         <> metavar "URL"
         <> help "URL of the central package index" )
    <.> flag (\a -> Right $ a { optCommand = Update (updateOpts a)
                                               { cleanCache = False } })
             (  short "c"
             <> long "cache"
             <> help "Do not clean global package cache" )

  uploadArgs =
       flag (\a -> Right $ a { optCommand =
                                 Upload (uploadOpts a) { setTag = False } })
            (  short "t"
            <> long "notagging"
            <> help "Do not tag git repository with current version" )
   <.> flag (\a -> Right $ a { optCommand =
                                 Upload (uploadOpts a) { forceUpdate = True } })
            (  short "f"
            <> long "force"
            <> help "Force, i.e., overwrite existing package version" )

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
                                  Doc (docOpts a) { docDir = Just s } })
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
    <.> option (\s a -> Right $ a { optCommand =
                                      Doc (docOpts a) { docPackageURL = s } })
          (  long "url"
          <> short "u"
          <> help ("The URL prefix where all repository packages are " ++
                   "documented. Default: " ++ defaultBaseDocURL)
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
                                                   { diffGroundEqu = True } })
         (  long "ground"
         <> short "g"
         <> help "Check ground equivalence only when comparing behavior")
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
            <> help "Add only dependency to the current package" )
   <.> flag (\a -> Right $ a { optCommand =
                                 Add (addOpts a) { forceAdd = True } })
            (  short "f"
            <> long "force"
            <> help "Force, i.e., overwrite existing package" )
   <.> arg (\s a -> Right $ a { optCommand =
                                  Add (addOpts a) { addSource = s } })
         (  metavar "PACKAGE"
         <> help "The package name (or directory for option '-p') to be added" )

-- Check if operating system executables we depend on are present on the
-- current system. Since this takes some time, it is only checked with
-- the `update` command.
checkRequiredExecutables :: IO ()
checkRequiredExecutables = do
  debugMessage "Checking whether all required executables can be found..."
  missingExecutables <- checkExecutables listOfExecutables
  unless (null missingExecutables) $ do
    errorMessage $ "The following programs could not be found on the PATH " ++
                   "(they are required for CPM to work):\n" ++
                   intercalate ", " missingExecutables
    exitWith 1
  debugMessage "All required executables found."
 where
  listOfExecutables = 
    [ "curl"  
    , "git"   
    , "unzip" 
    , "tar"
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
    then getBaseRepository cfg >>= \repo ->
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
updateCmd :: UpdateOptions -> Config -> IO (ErrorLogger ())
updateCmd opts cfg = do
  let cfg' = if null (indexURLs opts)
               then cfg
               else cfg { packageIndexURL = head (indexURLs opts) }
                    -- TODO: allow merging from several package indices
  checkRequiredExecutables >> updateRepository cfg' (cleanCache opts)

------------------------------------------------------------------------------
-- `deps` command:
depsCmd :: DepsOptions -> Config -> IO (ErrorLogger ())
depsCmd opts cfg =
  getLocalPackageSpec cfg "." |>= \specDir ->
  loadPackageSpec specDir |>= \pkg ->
  checkCompiler cfg pkg >>
  if depsPath opts -- show CURRYPATH only?
    then getCurryLoadPath cfg specDir |>= \loadpath ->
         putStrLn loadpath >> succeedIO ()
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
checkoutCmd :: CheckoutOptions -> Config -> IO (ErrorLogger ())
checkoutCmd (CheckoutOptions pkgname Nothing pre) cfg =
 getRepoForPackages cfg [pkgname] >>= \repo ->
 case findAllVersions repo pkgname pre of
  [] -> packageNotFoundFailure pkgname
  ps -> case filter (isCompatibleToCompiler cfg) ps of
    []    -> compatPackageNotFoundFailure cfg pkgname useUpdateHelp
    (p:_) -> acquireAndInstallPackageWithDependencies cfg repo p |>
             checkoutPackage cfg p
checkoutCmd (CheckoutOptions pkgname (Just ver) _) cfg =
 getRepoForPackages cfg [pkgname] >>= \repo ->
 case findVersion repo pkgname ver of
  Nothing -> packageNotFoundFailure $ pkgname ++ "-" ++ showVersion ver
  Just  p -> acquireAndInstallPackageWithDependencies cfg repo p |>
             checkoutPackage cfg p

installCmd :: InstallOptions -> Config -> IO (ErrorLogger ())
installCmd (InstallOptions Nothing Nothing _ instexec False) cfg =
  getLocalPackageSpec cfg "." |>= \pkgdir ->
  cleanCurryPathCache pkgdir |>
  installLocalDependencies cfg pkgdir |>= \ (pkg,_) ->
  saveBaseVersionToCache cfg pkgdir >>
  getCurryLoadPath cfg pkgdir |>= \currypath ->
  writePackageConfig cfg pkgdir pkg currypath |>
  if instexec then installExecutable cfg pkg else succeedIO ()
-- Install executable only:
installCmd (InstallOptions Nothing Nothing _ _ True) cfg =
  getLocalPackageSpec cfg "." |>= \pkgdir ->
  loadPackageSpec pkgdir |>= \pkg ->
  installExecutable cfg pkg
installCmd (InstallOptions (Just pkg) vers pre _ _) cfg = do
  fileExists <- doesFileExist pkg
  if fileExists
    then installFromZip cfg pkg
    else installApp (CheckoutOptions pkg vers pre) cfg
installCmd (InstallOptions Nothing (Just _) _ _ _) _ =
  failIO "Must specify package name"

--- Installs the application (i.e., binary) provided by a package.
--- This is done by checking out the package into CPM's application packages
--- cache (default: $HOME/.cpm/app_packages, see APP_PACKAGE_PATH
--- in .cpmrc configuration file) and then install this package.
---
--- Internal note: the installed package should not be cleaned or removed
--- after the installation since its execution might refer (via the
--- config module) to some data stored in the package.
installApp :: CheckoutOptions -> Config -> IO (ErrorLogger ())
installApp opts cfg = do
  let apppkgdir = appPackageDir cfg
      copname   = coPackage opts
      copkgdir  = apppkgdir </> coPackage opts
  curdir <- getCurrentDirectory
  removeDirectoryComplete copkgdir
  debugMessage ("Change into directory " ++ apppkgdir)
  inDirectory apppkgdir
    ( checkoutCmd opts cfg |>
      log Debug ("Change into directory " ++ copkgdir) |>
      (setCurrentDirectory copkgdir >> succeedIO ()) |>
      loadPackageSpec "." |>= \pkg ->
      maybe
       (setCurrentDirectory curdir >>
        removeDirectoryComplete copkgdir >>
        failIO ("Package '" ++ name pkg ++
                "' has no executable, nothing installed.\n" ++
                "Hint: use 'cypm add " ++ copname ++
                "' to add new dependency and install it."))
       (\_ -> installCmd (InstallOptions Nothing Nothing False True False) cfg)
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
           let (cmpname,_,_,_) = compilerVersion cfg
               cmd = unwords $
                       [":set", if levelGte Debug lvl then "v1" else "v0"
                       , maybe "" id (lookup cmpname eopts)
                       , ":load", mainmod, ":save", ":quit"]
               bindir     = binInstallDir cfg
               binexec    = bindir </> name
           in compiler (ExecOptions cmd) cfg |>
              log Info ("Installing executable '" ++ name ++ "' into '" ++
                        bindir ++ "'") |>
              (-- renaming might not work across file systems, hence we move:
               showExecCmd (unwords ["mv", mainmod, binexec]) >>
               checkPath path bindir))
        (executableSpec pkg)
 where
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
listCmd :: ListOptions -> Config -> IO (ErrorLogger ())
listCmd (ListOptions lv csv cat) cfg = do
  repo <- if cat then getRepositoryWithNameVersionCategory cfg
                 else getRepositoryWithNameVersionSynopsis cfg
  let listresult = if cat then renderCats (catgroups repo)
                          else renderPkgs (allpkgs repo)
  putStr listresult >> succeedIO ()
 where
  -- all packages (and versions if `lv`)
  allpkgs repo = concatMap (if lv then id else ((:[]) . filterCompatPkgs cfg))
                    (sortBy (\ps1 ps2 -> name (head ps1) <= name (head ps2))
                            (listPackages repo))

  -- all categories together with their package names:
  catgroups repo =
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
searchCmd :: SearchOptions -> Config -> IO (ErrorLogger ())
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
      (colsizes,rows) = packageVersionAsTable cfg results
  putStr $ unlines $
             if null results
               then [ "No packages found for '" ++ q ++ "'", useUpdateHelp ]
               else [ render (table rows colsizes), cpmInfo, useUpdateHelp ]
  succeedIO ()


--- `upgrade` command.
upgradeCmd :: UpgradeOptions -> Config -> IO (ErrorLogger ())
upgradeCmd (UpgradeOptions Nothing) cfg =
  getLocalPackageSpec cfg "." |>= \specDir ->
  cleanCurryPathCache specDir |>
  log Info "Upgrading all packages" |>
  upgradeAllPackages cfg specDir
upgradeCmd (UpgradeOptions (Just pkg)) cfg =
  getLocalPackageSpec cfg "." |>= \specDir ->
  log Info ("Upgrade " ++ pkg) |>
  upgradeSinglePackage cfg specDir pkg


--- `link` command.
linkCmd :: LinkOptions -> Config -> IO (ErrorLogger ())
linkCmd (LinkOptions src) cfg =
  getLocalPackageSpec cfg "." |>= \specDir ->
  cleanCurryPathCache specDir |>
  log Info ("Linking '" ++ src ++ "' into local package cache...") |>
  linkToLocalCache cfg src specDir

--- `add` command:
--- Option `--package`: copy the given package to the repository index
--- and package installation directory so that it is available as
--- any other package.
--- Option `--dependency`: add the package name as a dependency to the
--- current package
--- No option: like `--package` followed by `install` command
addCmd :: AddOptions -> Config -> IO (ErrorLogger ())
addCmd (AddOptions addpkg adddep pkg force) config
  | addpkg    = addPackageToRepository config pkg force True
  | adddep    = addDependencyCmd pkg force config
  | otherwise = addDependencyCmd pkg force config |>
                installCmd (installOpts defaultOptions) config


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
        newdeps   = addDep [[VGte vers, VLt (nextMajor vers)]]
                           (dependencies pkgSpec)
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
  (if docManual opts then genPackageManual pkg specDir absdocdir
                     else succeedIO ()) |>
    (if docPrograms opts then genDocForPrograms opts cfg absdocdir specDir pkg
                         else succeedIO ())

--- Generate manual according to  documentation specification of package.
genPackageManual :: Package -> String -> String -> IO (ErrorLogger ())
genPackageManual pkg specDir outputdir = case documentation pkg of
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
          inDirectory (specDir </> docdir) $ system formatcmd
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
    then log Info "No modules to be documented!"
    else
      getCurryLoadPath cfg specDir |>= \currypath ->
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
          in docPackageURL opts ++ "/" ++ d
    in map (\ip -> (ip,impPkg2URL ip)) (filter isImportedPackage dirs) ++
       if name pkg == "base"
         then [] -- in order to generate base package documentation
         else [(installDir </> "lib",
                docPackageURL opts ++ "/base-" ++ compilerBaseVersion cfg)]

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
      then log Info "No modules to be tested!"
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


diffCmd :: DiffOptions -> Config -> IO (ErrorLogger ())
diffCmd opts cfg =
  getLocalPackageSpec cfg "." |>= \specDir ->
  loadPackageSpec specDir     |>= \localSpec ->
  checkCompiler cfg localSpec >>
  let localname  = name localSpec
      localv     = version localSpec
      showlocalv = showVersion localv
  in
  getRepoForPackageSpec cfg localSpec >>= \repo ->
  getDiffVersion repo localname |>= \diffv ->
  if diffv == localv
    then failIO $ "Cannot diff identical package versions " ++ showlocalv
    else putStrLn ("Comparing local version " ++ showlocalv ++
                   " and repository version " ++ showVersion diffv ++ ":\n") >>
         installIfNecessary repo localname diffv |> putStrLn "" >>
         readGlobalCache cfg repo |>= \gc ->
         diffAPIIfEnabled      repo gc specDir localSpec diffv |> 
         diffBehaviorIfEnabled repo gc specDir localSpec diffv
 where
  getDiffVersion repo localname = case diffVersion opts of
    Nothing -> case findLatestVersion cfg repo localname False of
      Nothing -> failIO $
        "No other version of local package '" ++ localname ++
        "' compatible to '" ++ showCompilerVersion cfg ++
        "' found in package repository."
      Just p  -> succeedIO (version p)
    Just v  -> succeedIO v

  installIfNecessary repo pkgname ver =
    case findVersion repo pkgname ver of
      Nothing -> packageNotFoundFailure $ pkgname ++ "-" ++ showVersion ver
      Just  p -> acquireAndInstallPackageWithDependencies cfg repo p

  diffAPIIfEnabled repo gc specDir localSpec diffversion =
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

  diffBehaviorIfEnabled repo gc specDir localSpec diffversion =
    if diffBehavior opts
      then (putStrLn "Preparing behavior diff...\n" >> succeedIO ()) |>
           BDiff.preparePackageAndDir cfg repo gc specDir (name localSpec)
                                                          diffversion |>=
           \i -> BDiff.diffBehavior cfg repo gc i (diffGroundEqu opts)
                                    (diffUseAna opts) (diffModules opts)
      else succeedIO ()

-- Implementation of the `curry` command.
compiler :: ExecOptions -> Config -> IO (ErrorLogger ())
compiler o cfg =
  getLocalPackageSpec cfg "." |>= \pkgdir ->
  loadPackageSpec pkgdir |>= \pkg ->
  checkCompiler cfg pkg >>
  execWithPkgDir
    (ExecOptions $ unwords [curryExec cfg, "--nocypm", exeCommand o])
    cfg pkgdir

-- Implementation of the `exec` command.
execCmd :: ExecOptions -> Config -> IO (ErrorLogger ())
execCmd o cfg =
  getLocalPackageSpec cfg "." |>= execWithPkgDir o cfg

execWithPkgDir :: ExecOptions -> Config -> String -> IO (ErrorLogger ())
execWithPkgDir o cfg specDir =
  getCurryLoadPath cfg specDir |>= execWithCurryPath o cfg

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


--- Creates a new package.
newPackage :: NewOptions -> IO (ErrorLogger ())
newPackage (NewOptions pname) = do
  exists <- doesDirectoryExist pname
  when exists $ do
    errorMessage $ "There is already a directory with the new project name.\n"
                ++ "I cannot create new project!"
    exitWith 1
  let emptyAuthor   = "YOUR NAME <YOUR EMAIL ADDRESS>"
      emptySynopsis = "PLEASE PROVIDE A ONE-LINE SUMMARY ABOUT THE PACKAGE"
  createDirectory pname
  let pkgSpec = emptyPackage { name            = pname
                             , version         = initialVersion
                             , author          = [emptyAuthor]
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

------------------------------------------------------------------------------
--- Uploads a package to package server.
uploadCmd :: UploadOptions -> Config -> IO (ErrorLogger ())
uploadCmd opts cfg =
  getLocalPackageSpec cfg "." |>= \specDir ->
  loadPackageSpec specDir |>= \lpkg ->
  let pkgrepodir = repositoryDir cfg </>
                   name lpkg </> showVersion (version lpkg) in
  doesDirectoryExist pkgrepodir >>= \exrepodir ->
  (if exrepodir && not (forceUpdate opts)
    then failIO $ "Package version already exists in repository!"
    else succeedIO () ) |>
  inDirectory specDir (setTagInGitIfNecessary opts lpkg) |>
  tempDir >>= \instdir ->
  recreateDirectory instdir >>
  installPkg lpkg instdir |>
  let pkgid = packageId lpkg in
  loadPackageSpec (instdir </> pkgid) |>= \pkg ->
  testPackage pkgid instdir >>= \ecode ->
  if ecode > 0
    then removeDirectoryComplete instdir >>
         log Critical "ERROR in package, package not uploaded!"
    else log Info "Uploading package to global repository..." |>
         removeInstalledPkg pkgid >>
         uploadPackageSpec (instdir </> pkgid </> "package.json") |>
         -- add package to local copy of repository:
         addPackageToRepo pkgrepodir (instdir </> pkgid) pkg >>
         removeDirectoryComplete instdir >>
         log Info ("Package '" ++ pkgid ++ "' uploaded")
 where
  addPackageToRepo pkgrepodir pkgdir pkg = do
    exrepodir <- doesDirectoryExist pkgrepodir
    infoMessage $ "Create directory: " ++ pkgrepodir
    createDirectoryIfMissing True pkgrepodir
    copyFile (pkgdir </> "package.json") (pkgrepodir </> "package.json")
    if exrepodir then updatePackageInRepositoryCache cfg pkg
                 else addPackageToRepositoryCache    cfg pkg

  -- TODO: check url
  installPkg pkg instdir = case source pkg of
    Nothing            -> failIO $ "No source specified for package"
    Just (Git url rev) -> installPackageSourceTo pkg (Git url rev) instdir
    _                  -> failIO $ "No git source with version tag"

  removeInstalledPkg pkgid = do
    let pkgInstallDir = packageInstallDir cfg </> pkgid
    expkgdir <- doesDirectoryExist pkgInstallDir
    when expkgdir $ removeDirectoryComplete pkgInstallDir

  testPackage pkgid instdir = do
    curdir <- inDirectory instdir getCurrentDirectory
    let bindir = curdir </> "pkgbin"
    recreateDirectory bindir
    let cmd = unwords
                [ -- install possible binaries in bindir:
                  "cypm", "-d bin_install_path="++bindir, "install", "&&"
                , "export PATH="++bindir++":$PATH", "&&"
                , "cypm", "test", "&&"
                , "cypm", "-d bin_install_path="++bindir, "uninstall"
                ]
    putStrLn $ "Testing package: '" ++ pkgid ++ "' with command:\n" ++ cmd
    inDirectory (instdir </> pkgid) $ system cmd

--- Set the package version as a tag in the local GIT repository and push it,
--- if the package source is a GIT with tag `$version`.
setTagInGitIfNecessary :: UploadOptions -> Package -> IO (ErrorLogger ())
setTagInGitIfNecessary opts pkg
  | not (setTag opts) = succeedIO ()
  | otherwise = case source pkg of
                  Just (Git _ (Just VersionAsTag)) -> setTagInGit pkg
                  _ -> succeedIO ()

setTagInGit :: Package -> IO (ErrorLogger ())
setTagInGit pkg = do
  let ts = 'v' : showVersion (version pkg)
  infoMessage $ "Tagging current git repository with tag '" ++ ts++ "'"
  (_,gittag,_) <- evalCmd "git" ["tag","-l",ts] ""
  let deltag = if null gittag then [] else ["git tag -d",ts,"&&"]
      cmd    = unwords $ deltag ++ ["git tag -a",ts,"-m",ts,"&&",
                                    "git push --tags -f"]
  infoMessage $ "Execute: " ++ cmd
  ecode <- system cmd
  if ecode == 0 then succeedIO ()
                else failIO $ "ERROR in setting the git tag"

-- Uploads a package specification stored in a file (first argument).
uploadPackageSpec :: String -> IO (ErrorLogger ())
uploadPackageSpec pkgspecfname = do
  pkgspec <- readFile pkgspecfname
  (rc,out,err) <- evalCmd "curl" ["--data-binary", "@-", uploadURL ] pkgspec
  unless (null out) $ infoMessage out
  if rc == 0
    then succeedIO ()
    else log Info err |> failIO "Adding to global repository failed!"

-- URL of cpm-upload script.
uploadURL :: String
--uploadURL = "http://localhost/~mh/cpm-upload.cgi"
uploadURL = "https://www-ps.informatik.uni-kiel.de/~mh/cpm-upload.cgi"

------------------------------------------------------------------------------
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

--- Gets CURRYPATH of the given package (either from the local cache file
--- in the package dir or compute it).
getCurryLoadPath :: Config -> String -> IO (ErrorLogger String)
getCurryLoadPath cfg pkgdir =
  loadCurryPathFromCache cfg pkgdir |>=
  maybe (computePackageLoadPath cfg pkgdir) succeedIO

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
