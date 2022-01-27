--------------------------------------------------------------------------------
--- This module defines the options of CPM and operations to process them.
--------------------------------------------------------------------------------

module CPM.Options
 where

import Prelude hiding   ( (<|>) )
import Data.Char        ( toLower )
import Data.List        ( splitOn )

import OptParse

import CPM.Package      ( Version, readVersion )
import CPM.ErrorLogger

------------------------------------------------------------------------------
-- The default URL prefix where all repository packages are documented.
-- Can be overwritten with a doc command option.
defaultBaseDocURL :: String
defaultBaseDocURL = "https://www-ps.informatik.uni-kiel.de/~cpm/DOC"

------------------------------------------------------------------------------

--- The global options of CPM.
data Options = Options
  { optLogLevel    :: LogLevel
  , optDefConfig   :: [(String,String)]
  , optShowVersion :: Bool
  , optWithTime    :: Bool
  , optCommand     :: Command }

--- The default options: no command, no timing, info log level
defaultOptions :: Options
defaultOptions = Options Info [] False False NoCommand

--- The CPM commands with their options.
data Command
  = NoCommand
  | Help
  | ConfigCmd  ConfigOptions
  | Deps       DepsOptions
  | Check      CheckOptions
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
  { depsPath  :: Bool  -- show CURRYPATH only?
  , depsGraph :: Bool  -- show dot graph instead of tree?
  , depsView  :: Bool  -- view dot graph with `dotviewcommand` of rc file?
  }

data CheckOptions = CheckOptions
  { chkInfo    :: Bool
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
  , testCompile   :: Bool            -- only compile modules?
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
  ConfigCmd opts -> opts
  _              -> ConfigOptions False

depsOpts :: Options -> DepsOptions
depsOpts s = case optCommand s of
  Deps opts -> opts
  _         -> DepsOptions False False False

checkOpts :: Options -> CheckOptions
checkOpts s = case optCommand s of
  Check opts -> opts
  _          -> CheckOptions False

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

testOpts :: Options -> TestOptions
testOpts s = case optCommand s of
  Test opts -> opts
  _         -> TestOptions Nothing False False "" []

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
        (   command "add"
              (help "Add a package (as dependency or to the local repository)")
              Right
              addArgs
        <|> command "check" (help "Check a package")
                    (\a -> Right $ a { optCommand = Check (checkOpts a) })
                    checkArgs
        <|> command "checkout" (help "Checkout a package") Right
                    (checkoutArgs Checkout)
        <|> command "clean" (help "Clean the current package")
                            (\a -> Right $ a { optCommand = Clean }) []
        <|>  command "config"
                    (help "Show current configuration of CPM")
                    (\a -> Right $ a { optCommand = ConfigCmd (configOpts a) })
                    configArgs
        <|> command "curry"
           (help "Load package spec and start Curry with correct dependencies.")
                 (\a -> Right $ a { optCommand = Compiler (execOpts a) })
                 curryArgs
        <|> command "deps" (help "Calculate dependencies")
                           (\a -> Right $ a { optCommand = Deps (depsOpts a) })
                           depsArgs
        <|> command "diff"
                    (help "Diff the current package against another version")
                    (\a -> Right $ a { optCommand = Diff (diffOpts a) })
                    diffArgs
        <|> command "doc"
           (help "Generation documentation for current package (with CurryDoc)")
                    (\a -> Right $ a { optCommand = Doc (docOpts a) })
                    docArgs
        <|> command "exec"
                    (help "Execute a command with the CURRYPATH set")
                    (\a -> Right $ a { optCommand = Exec (execOpts a) })
                    execArgs
        <|> command "help" (help "Show list of all commands and quit")
                           (\a -> Right $ a { optCommand = Help }) []
        <|> command "info" (help "Print package information")
                    (\a -> Right $ a { optCommand = PkgInfo (infoOpts a) })
                    infoArgs
        <|> command "install" (help "Install a package with its dependencies.")
                     (\a -> Right $ a { optCommand = Install (installOpts a) })
                     installArgs
        <|> command "link" (help "Link a package to the local cache") Right
                    linkArgs
        <|> command "list" (help "List all packages of the repository")
                    (\a -> Right $ a { optCommand = List (listOpts a) })
                    listArgs
        <|> command "new" (help "Create a new package") Right newArgs
        <|> command "search" (help "Search the package repository") Right
                    searchArgs
        <|> command "test" (help "Test the current package (with CurryCheck)")
                    (\a -> Right $ a { optCommand = Test (testOpts a) })
                    testArgs
        <|> command "uninstall" (help "Uninstall a package")
                 (\a -> Right $ a { optCommand = Uninstall (uninstallOpts a) })
                 uninstallArgs
        <|> command "upgrade" (help "Upgrade one or more packages")
                    (\a -> Right $ a { optCommand = Upgrade (upgradeOpts a) })
                    upgradeArgs
        <|> command "update"
                    (help "Update the package index")
                    (\a -> Right $ a { optCommand = Update (updateOpts a) })
                    updateArgs
        <|> command "upload"
                    (help "Upload current package to package server")
                    (\a -> Right $ a { optCommand = Upload (uploadOpts a) })
                    uploadArgs
        ) )
 where
  configArgs =
    flag (\a -> Right $ a { optCommand = ConfigCmd (configOpts a)
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
    <.> flag (\a -> Right $ a { optCommand = Deps (depsOpts a)
                                              { depsGraph = True } })
             (  short "g"
             <> long "graph"
             <> help "Show dependencies as dot graph (in Graphviz format)"
             <> optional )
    <.> flag (\a -> Right $ a { optCommand = Deps (depsOpts a)
                                              { depsView = True } })
             (  short "v"
             <> long "viewgraph"
             <> help "View dependency graph (with 'dotviewcommand')"
             <> optional )

  checkArgs =
     flag (\a -> Right $ a { optCommand = Check (checkOpts a)
                                                { chkInfo = True } })
          (  short "i"
          <> long "info"
          <> help "Show more information about the package"
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
                              Test (testOpts a) { testCompile = True } })
         (  short "c"
         <> long "compile"
         <> help "Only compile modules (no tests with CurryCheck)"
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

------------------------------------------------------------------------------
