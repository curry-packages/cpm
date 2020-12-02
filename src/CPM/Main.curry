--------------------------------------------------------------------------------
--- This is the main module of the Curry Package Manager.
--------------------------------------------------------------------------------

module CPM.Main ( main )
 where

import Data.Char           ( toLower )
import Data.List           ( groupBy, intercalate, isPrefixOf, isSuffixOf
                           , nub, split, sortBy, splitOn )
import Data.Either
import Data.Time           ( calendarTimeToString, getLocalTime )
import System.Directory    ( doesFileExist, getAbsolutePath, doesDirectoryExist
                           , copyFile, createDirectory, createDirectoryIfMissing
                           , getCurrentDirectory, getDirectoryContents
                           , getModificationTime
                           , renameFile, removeFile, setCurrentDirectory )
import System.FilePath     ( (</>), splitSearchPath, replaceExtension
                           , takeExtension, pathSeparator, isPathSeparator )
import System.IO           ( hFlush, stdout )
import System.Environment  ( getArgs, getEnv, setEnv, unsetEnv )
import System.Process      ( exitWith, system, getPID )
import Control.Monad       ( when, unless, foldM )
import System.IOExts       ( evalCmd, readCompleteFile )
import Language.Curry.Distribution ( installDir )
import Prelude hiding      ( (<|>) )

import Boxes            ( table, render )
import OptParse
import System.CurryPath ( addCurrySubdir, stripCurrySuffix )
import System.Path      ( fileInPath, getFileInPath )
import Text.CSV         ( readCSV, showCSV, writeCSVFile )

import CPM.ErrorLogger
import CPM.FileUtil ( cleanTempDir, joinSearchPath, safeReadFile, whenFileExists
                    , inDirectory, recreateDirectory
                    , removeDirectoryComplete, copyDirectory, quote, tempDir )
import CPM.Config   ( Config (..)
                    , readConfigurationWith, showCompilerVersion
                    , showConfiguration )
import CPM.PackageCache.Global ( acquireAndInstallPackage
                               , GlobalCache, readGlobalCache, allPackages
                               , checkoutPackage
                               , installFromZip, installedPackageDir
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
import CPM.ConfigPackage        ( packagePath, packageVersion )

-- Banner of this tool:
cpmBanner :: String
cpmBanner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText =
    "Curry Package Manager <curry-lang.org/tools/cpm> (Version " ++
    packageVersion ++ ", 02/12/2020)"
  bannerLine = take (length bannerText) (repeat '-')

main :: IO ()
main = do
  args <- getArgs
  if "-V" `elem` args || "--version" `elem` args
    then putStrLn $ "Curry Package Manager, version " ++ packageVersion
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
      logDebug ("Current configuration:\n" ++ showConfiguration config)
      case optCommand opts of
        NoCommand   -> fail "NoCommand"
        Config o    -> configCmd    o config
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
        New o       -> newCmd       o
        List      o -> listCmd      o config
        Search    o -> searchCmd    o config
        Upgrade   o -> upgradeCmd   o config
        Diff      o -> diffCmd      o config
        Checkout  o -> checkoutCmd  o config
        Install   o -> installCmd   o config
        Upload    o -> uploadCmd    o config
        Clean       -> cleanPackage config Info
  mapM (showLogEntry ll) msgs
  let allOk =  all (levelGte Info) (map logLevelOf msgs) &&
               either (\le -> levelGte Info (logLevelOf le))
                      (const True)
                      result
  exitWith $ if allOk then 0 else 1
 where runErrorLogger' a b c = runErrorLogger c a b

-- The global options of CPM.
data Options = Options
  { optLogLevel    :: LogLevel
  , optDefConfig   :: [(String,String)]
  , optShowVersion :: Bool
  , optWithTime    :: Bool
  , optCommand     :: Command }

-- The default options: no command, no timing, info log level
defaultOptions :: Options
defaultOptions = Options Info [] False False NoCommand

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
  { listVers   :: Bool   -- list all versions of each package
  , listSystem :: Bool   -- list package compatible with current Curry system
  , listCSV    :: Bool   -- list in CSV format
  , listCat    :: Bool   -- list all categories
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
  { indexURLs     :: [String]   -- the URLs of additional index repositories
  , cleanCache    :: Bool       -- clean also repository cache?
  , downloadIndex :: Bool       -- download the index repository?
  , useRepoCache  :: Bool       -- use repository cache to create repository DB?
  , writeCSV      :: Bool       -- write also a CSV file of the repository DB?
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
  , docReadme     :: Bool            -- generate README as HTML
  , docPrograms   :: Bool            -- generate documentation for programs
  , docManual     :: Bool            -- generate manual (if specified)
  , docGenImports :: Bool            -- generate documentation for imported pkgs
                                     -- (otherwise, use their standard docs)
  , docPackageURL :: String          -- the URL prefix where all repository
                                     -- packages are documented
}

data TestOptions = TestOptions
  { testModules   :: Maybe [String]  -- modules to be tested
  , testSafe      :: Bool            -- safe test? (no scripts, no I/O props)
  , testFile      :: String          -- file to write test statistics as CSV
  , testCheckOpts :: [String]        -- additional options passed to CurryCheck
  }

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
  _         -> ListOptions False False False False

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
  _           -> UpdateOptions [] True True True False

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
  _        -> DocOptions Nothing Nothing True True True False defaultBaseDocURL

-- The default URL prefix where all repository packages are documented.
-- Can be overwritten with a doc command option.
defaultBaseDocURL :: String
defaultBaseDocURL = "https://www-ps.informatik.uni-kiel.de/~cpm/DOC"


testOpts :: Options -> TestOptions
testOpts s = case optCommand s of
  Test opts -> opts
  _         -> TestOptions Nothing False "" []

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
  (   flag (\a -> Right $ a { optShowVersion = True })
        (  long "version"
        <> short "V"
        <> help "Show version and quit" )
  <.> option (\s a -> readLogLevel s >.> \ll -> a { optLogLevel = ll })
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
             <> long "clean"
             <> help "Do not clean global package cache" )
    <.> flag (\a -> Right $ a { optCommand = Update (updateOpts a)
                                               { downloadIndex = False } })
             (  short "d"
             <> long "download"
             <> help "Do not download the global repository index" )
    <.> flag (\a -> Right $ a { optCommand = Update (updateOpts a)
                                               { useRepoCache = False } })
             (  short "n"
             <> long "nocache"
             <> help "Do not download global repository cache files" )
    <.> flag (\a -> Right $ a { optCommand = Update (updateOpts a)
                                               { writeCSV = True } })
             (  short "w"
             <> long "writecsv"
             <> help "Write also a CSV file of the cache database" )

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
                                  { docManual = False, docPrograms = False } })
          (  short "r"
          <> long "readme"
          <> help "Generate only README as HTML"
          <> optional )
    <.> flag (\a -> Right $ a { optCommand = Doc (docOpts a)
                                  { docManual = False, docReadme = False } })
          (  short "p"
          <> long "programs"
          <> help "Generate only program documentation (with CurryDoc)"
          <> optional )
    <.> flag (\a -> Right $ a { optCommand = Doc (docOpts a)
                                  { docPrograms = False, docReadme = False } })
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
    <.>
    flag (\a -> Right $ a { optCommand =
                              Test (testOpts a) { testSafe = True } })
         (  short "s"
         <> long "safe"
         <> help "Safe test mode (no script tests, no I/O tests)"
         <> optional )
    <.>
    option (\s a -> Right $ a { optCommand =
                                  Test (testOpts a) { testFile = s } })
          (  long "file"
          <> short "f"
          <> help "File to store test statistics in CSV format"
          <> optional )
    <.>
    option (\s a -> Right $ a { optCommand =
                                  Test (testOpts a) { testCheckOpts =
                                            s : testCheckOpts (testOpts a) } })
          (  long "option"
          <> short "o"
          <> help "Option passed to CurryCheck (without prefix '--'!)"
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
          <> help "List all versions" )
    <.> flag (\a -> Right $ a { optCommand =
                                  List (listOpts a) { listSystem = True } })
          (  short "s"
          <> long "system"
          <> help "List packages compatible with current compiler" )
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
checkRequiredExecutables :: ErrorLogger ()
checkRequiredExecutables = do
  logDebug "Checking whether all required executables can be found..."
  missingExecutables <- liftIOEL $ checkExecutables listOfExecutables
  unless (null missingExecutables) $ do
    logError $ "The following programs could not be found on the PATH " ++
                   "(they are required for CPM to work):\n" ++
                   intercalate ", " missingExecutables
    liftIOEL $ exitWith 1
  logDebug "All required executables found."
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
  present <- mapM fileInPath executables
  return $ map fst $ filter (not . snd) (zip executables present)

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
  if depsPath opts -- show CURRYPATH only?
    then do loadpath <- getCurryLoadPath cfg specDir
            putStrLnELM loadpath
    else do result <- resolveDependencies cfg specDir
            putStrLnELM (showResult result)

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
-- `checkout` command:
checkoutCmd :: CheckoutOptions -> Config -> ErrorLogger ()
checkoutCmd (CheckoutOptions pkgname Nothing pre) cfg = do
 repo <- getRepoForPackages cfg [pkgname]
 case findAllVersions repo pkgname pre of
  [] -> packageNotFoundFailure pkgname
  ps -> case filter (isCompatibleToCompiler cfg) ps of
    []    -> compatPackageNotFoundFailure cfg pkgname useUpdateHelp
    (p:_) -> do acquireAndInstallPackageWithDependencies cfg repo p
                checkoutPackage cfg p
checkoutCmd (CheckoutOptions pkgname (Just ver) _) cfg = do
 repo <- getRepoForPackages cfg [pkgname]
 case findVersion repo pkgname ver of
  Nothing -> packageNotFoundFailure $ pkgname ++ "-" ++ showVersion ver
  Just  p -> do acquireAndInstallPackage cfg p
                checkoutPackage cfg p

installCmd :: InstallOptions -> Config -> ErrorLogger ()
installCmd (InstallOptions Nothing Nothing _ instexec False) cfg = do
  pkgdir <- getLocalPackageSpec cfg "."
  cleanCurryPathCache pkgdir
  (pkg,_) <- installLocalDependencies cfg pkgdir
  currypath <- getCurryLoadPath cfg pkgdir
  writePackageConfig cfg pkgdir pkg currypath
  when instexec $ installExecutable cfg pkg
-- Install executable only:
installCmd (InstallOptions Nothing Nothing _ _ True) cfg = do
  pkgdir <- getLocalPackageSpec cfg "."
  pkg    <- loadPackageSpec pkgdir
  installExecutable cfg pkg
installCmd (InstallOptions (Just pkg) vers pre _ _) cfg = do
  fileExists <- liftIOEL $ doesFileExist pkg
  if fileExists
    then installFromZip cfg pkg
    else installApp (CheckoutOptions pkg vers pre) cfg
installCmd (InstallOptions Nothing (Just _) _ _ _) _ =
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
installExecutable :: Config -> Package -> ErrorLogger ()
installExecutable cfg pkg = do
  checkCompiler cfg pkg
  mapM_ (\ (PackageExecutable name mainmod eopts) -> do
           lvl <- getLogLevel
           path <- liftIOEL $ getEnv "PATH"
           logInfo $ "Compiling main module: " ++ mainmod
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
        (executableSpec pkg)
 where
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
      then do liftIOEL $ writePackageSpec newpkg (pkgdir </> "package.json")
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
  absdocdir <- liftIOEL $ getAbsolutePath docdir
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
      rc1 <- inDirectoryEL specDir $ liftIOEL $ system formatcmd1
      logDebug $ "Executing command: " ++ formatcmd2
      rc2 <- inDirectoryEL specDir $ liftIOEL $ system formatcmd2
      if rc1 == 0 && rc2 == 0
        then do
          -- make them readable:
          liftIOEL $ system $
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

  formatCmd1 readme = "pandoc -s -t html -o " ++ outfile1 ++ " " ++ readme
  formatCmd2 readme = "pandoc -t html -o " ++ outfile2 ++ " " ++ readme

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
          rc <- inDirectoryEL (specDir </> docdir) $ liftIOEL $ system formatcmd
          if rc == 0
            then do
              let outfile = outputdir </> replaceExtension docmain ".pdf"
               -- make it readable:
              liftIOEL $ system ("chmod -f 644 " ++ quote outfile)
              logInfo $
                "Package documentation written to '" ++ outfile ++ "'."
            else fail $ "Error during execution of command:\n" ++ formatcmd
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
--- run `curry-doc` on the modules provided as an argument
--- or, if they are not given, on exported modules (if specified in the
--- package), on the main executable (if specified in the package),
--- or on all source modules of the package.
genDocForPrograms :: DocOptions -> Config -> String -> String -> Package
                  -> ErrorLogger ()
genDocForPrograms opts cfg docdir specDir pkg = do
  abspkgdir <- liftIOEL $ getAbsolutePath specDir
  checkCompiler cfg pkg
  let exports  = exportedModules pkg
      mainmods = map (\ (PackageExecutable _ emain _) -> emain)
                     (executableSpec pkg)
  (docmods,apidoc) <-
     maybe (if null exports
              then if null mainmods
                     then (do ms <- liftIOEL $
                                     curryModulesInDir (specDir </> "src")
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
  apititle = "\"API Documentation of Package '" ++ name pkg ++ "'\""

  getCurryDoc = do
    mbf <- liftIOEL $ getFileInPath cdbin
    maybe (do let cpmcurrydoc = binInstallDir cfg </> cdbin
              cdex <- liftIOEL $ doesFileExist cpmcurrydoc
              if cdex then return cpmcurrydoc
                      else fail $ "Executable '" ++ cdbin ++ "' not found!"
          )
          return
          mbf
   where cdbin = "curry-doc"

  docModule currypath uses mod =
    runDocCmd currypath uses ["--noindexhtml", docdir, mod]

  runDocCmd currypath uses docparams = do
    currydoc <- getCurryDoc
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
  aspecDir <- liftIOEL $ getAbsolutePath specDir
  mainprogs <- liftIOEL $ curryModulesInDir (aspecDir </> "src")
  let tests = testSuites pkg mainprogs
  stats <- if null tests
             then do logInfo "No modules to be tested!"
                     return []
             else mapM (execTest aspecDir) tests
  unless (null (testFile opts)) $ liftIOEL $
    combineCSVStatsOfPkg (packageId pkg) (concat stats) (testFile opts)
 where
  getCurryCheck = do
    mbf <- liftIOEL $ getFileInPath ccbin
    maybe (do let cpmcurrycheck = binInstallDir cfg </> ccbin
              ccex <- liftIOEL $ doesFileExist cpmcurrycheck
              if ccex then return cpmcurrycheck
                      else fail $ "Executable '" ++ ccbin ++ "' not found!"
          )
          return
          mbf
   where ccbin = "curry-check"

  execTest apkgdir (PackageTest dir mods pccopts script) = do
    currycheck <- getCurryCheck
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
  ltime <- getLocalTime
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


--- Get the names of all Curry modules contained in a directory.
--- Modules in subdirectories are returned as hierarchical modules.
curryModulesInDir :: String -> IO [String]
curryModulesInDir dir = getModules "" dir
 where
  getModules p d = do
    exdir <- doesDirectoryExist d
    entries <- if exdir then getDirectoryContents d else return []
    let realentries = filter (\f -> length f >= 1 && head f /= '.') entries
        newprogs    = filter (\f -> takeExtension f == ".curry") realentries
    subdirs <- mapM (\e -> do b <- doesDirectoryExist (d </> e)
                              return $ if b then [e] else [])
                    realentries
               >>= return . concat
    subdirentries <- mapM (\s -> getModules (p ++ s ++ ".") (d </> s)) subdirs
    return $ map ((p ++) . stripCurrySuffix) newprogs ++ concat subdirentries


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

computePackageLoadPath :: Config -> String -> ErrorLogger String
computePackageLoadPath cfg pkgdir = do
  logDebug "Computing load path for package..."
  pkg <- loadPackageSpec pkgdir
  allpkgs <- resolveAndCopyDependenciesForPackage cfg pkgdir pkg
  abs <- liftIOEL $ getAbsolutePath pkgdir
  let srcdirs = map (abs </>) (sourceDirsOf pkg)
      -- remove 'base' package since it is in the compiler libraries:
      pkgs = filter (\p -> name p /= "base") allpkgs
      currypath = joinSearchPath (srcdirs ++ dependencyPathsSeparate pkgs abs)
  liftIOEL $ saveCurryPathToCache cfg pkgdir currypath
  return currypath


--- Implementation of the `new` command: create a new package.
newCmd :: NewOptions -> ErrorLogger ()
newCmd (NewOptions pname) = do
  exists <- liftIOEL $ doesDirectoryExist pname
  when exists $ do
    logError $ "There is already a directory with the new project name.\n"
                ++ "I cannot create new project!"
    liftIOEL $ exitWith 1
  liftIOEL $ do
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
    let licenseFile = packagePath </> "templates" </> "LICENSE"
    whenFileExists licenseFile $ copyFile licenseFile (pname </> "LICENSE")
    createDirectory (pname </> "src")
    let cmain    = "Main.curry"
        mainFile = packagePath </> "templates" </> cmain
    whenFileExists mainFile $ copyFile mainFile (pname </> "src" </> cmain)
    writeFile (pname </> "README.md") readme
    writeFile (pname </> ".gitignore") gitignore
    putStr $ unlines todo
 where
  readme = unlines [pname, take (length pname) (repeat '=')]
  gitignore = unlines ["*~", ".cpm", ".curry"]

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
      logInfo "Uploading package to global repository..."
      -- remove package possibly existing in global package cache:
      liftIOEL $ removeDirectoryComplete (installedPackageDir cfg pkg)
      uploadPackageSpec (instdir </> pkgid </> "package.json")
      addPackageToRepo pkgrepodir (instdir </> pkgid) pkg
      liftIOEL $ cleanTempDir
      logInfo $ "Package '" ++ pkgid ++ "' uploaded"
 where
  -- add package to local copy of repository:
  addPackageToRepo pkgrepodir pkgdir pkg = do
    exrepodir <- liftIOEL $ doesDirectoryExist pkgrepodir
    logInfo $ "Create directory: " ++ pkgrepodir
    liftIOEL $ do
      createDirectoryIfMissing True pkgrepodir
      copyFile (pkgdir </> "package.json") (pkgrepodir </> "package.json")
    if exrepodir then updatePackageInRepositoryCache cfg pkg
                 else addPackageToRepositoryCache    cfg pkg

  -- TODO: check url
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
                , "export PATH="++bindir++":$PATH", "&&"
                , "cypm", "test", "&&"
                , "cypm", "-d bin_install_path="++bindir, "uninstall"
                ]
    putStrLnELM $ "Testing package: '" ++ pkgid ++ "' with command:\n" ++ cmd
    inDirectoryEL (instdir </> pkgid) $ liftIOEL $ system cmd

--- Set the package version as a tag in the local GIT repository and push it,
--- if the package source is a GIT with tag `$version`.
setTagInGitIfNecessary :: UploadOptions -> Package -> ErrorLogger ()
setTagInGitIfNecessary opts pkg
  | not (setTag opts) = return ()
  | otherwise = case source pkg of
                  Just (Git _ (Just VersionAsTag)) -> setTagInGit pkg
                  _                                -> return ()

setTagInGit :: Package -> ErrorLogger ()
setTagInGit pkg = do
  let ts = 'v' : showVersion (version pkg)
  logInfo $ "Tagging current git repository with tag '" ++ ts++ "'"
  (_,gittag,_) <- liftIOEL $ evalCmd "git" ["tag","-l",ts] ""
  let deltag = if null gittag then [] else ["git tag -d",ts,"&&"]
      cmd    = unwords $ deltag ++ ["git tag -a",ts,"-m",ts,"&&",
                                    "git push --tags -f"]
  logInfo $ "Execute: " ++ cmd
  ecode <- liftIOEL $ system cmd
  if ecode == 0 then return ()
                else fail $ "ERROR in setting the git tag"

-- Uploads a package specification stored in a file (first argument,
-- like `.../package.json`) with the web script specified by `uploadURL`.
uploadPackageSpec :: String -> ErrorLogger ()
uploadPackageSpec pkgspecfname = do
  pkgspec <- liftIOEL $ readFile pkgspecfname
  let curlopts = ["--data-binary", "@-", uploadURL ]
  logDebug $ unwords ("curl" : curlopts) ++ "\n" ++ pkgspec
  (rc,out,err) <- liftIOEL $ evalCmd "curl" curlopts pkgspec
  unless (null out) $ logInfo out
  if rc == 0
    then return ()
    else do logInfo err
            fail "Adding to global repository failed!"

-- URL of cpm-upload script.
uploadURL :: String
uploadURL = "https://www-ps.informatik.uni-kiel.de/~cpm/cpm-upload.cgi"

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
--- in the package dir or compute it).
getCurryLoadPath :: Config -> String -> ErrorLogger String
getCurryLoadPath cfg pkgdir = do
  mbp <- loadCurryPathFromCache cfg pkgdir
  maybe (computePackageLoadPath cfg pkgdir) return mbp

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
      pftime <- liftIOEL $ getModificationTime (pkgdir </> "package.json")
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
