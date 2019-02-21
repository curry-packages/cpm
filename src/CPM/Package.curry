------------------------------------------------------------------------------
--- This module contains the data types for a package specification and
--- versions as well as functions for reading/showing/comparing package specs
--- and package versions.
------------------------------------------------------------------------------

module CPM.Package
  ( Version, initialVersion, nextMajor, nextMinor
  , VersionConstraint (..)
  , CompilerCompatibility (..)
  , Package (..), emptyPackage
  , Dependency (..)
  , execOfPackage
  , showVersion
  , replaceVersionInTag
  , readVersion
  , packageIdEq
  , showPackageSource
  , readVersionConstraint
  , readVersionConstraints
  , readPackageSpec
  , sourceDirsOf
  , dependencyNames
  , vlt
  , vlte
  , vgt
  , vgte
  , isPreRelease
  , packageId
  , PackageId (..)
  , PackageSource (..)
  , GitRevision (..)
  , PackageExecutable (..), PackageTest (..), PackageDocumentation (..)
  , showDependency
  , showCompilerDependency
  , loadPackageSpec
  , writePackageSpec
  , Conjunction
  , Disjunction
  , packageSpecToJSON
  ) where

import Char
import Either
import FilePath    ( (</>) )
import IOExts      ( readCompleteFile )
import JSON.Data
import JSON.Parser
import JSON.Pretty ( ppJSON )
import List        ( intercalate, intersperse, isInfixOf, splitOn )
import Read        ( readInt )
import Test.Prop

import DetParse

import CPM.ErrorLogger
import CPM.FileUtil (ifFileExists)

--- Data type representin a version number.
--- It is a tuple where the components are major, minor, patch, prerelease,
--- e.g., 3.1.1-rc5
type Version = (Int, Int, Int, Maybe String)

--- The initial version of a new package.
initialVersion :: Version
initialVersion = (0,0,1,Nothing)

--- The next major version of a given version.
nextMajor :: Version -> Version
nextMajor (maj,_,_,_) = (maj + 1, 0, 0, Nothing)

--- The next minor version of a given version.
nextMinor :: Version -> Version
nextMinor (maj,min,_,_) = (maj, min + 1, 0, Nothing)

type Conjunction = [VersionConstraint]
type Disjunction = [Conjunction]

--- A dependency on another package. The disjunctive normal form of a boolean
--- combination of version constraints is represented by a list of lists of
--- version constraint. Each inner list of version constraints is a conjunction,
--- the outer list is a disjunction.
data Dependency = Dependency String Disjunction
 deriving (Eq,Show)

--- A version constraint.
--- @cons VExact - versions must match exactly
--- @cons VGt - version must be strictly larger than specified version
--- @cons VLt - version must be strictly smaller than specified version
--- @cons VGte - version must be larger or equal to specified version
--- @cons VLte - version must be smaller or equal to specified version
--- @cons VCompatible - semver arrow, version must be larger or equal and
---                     within same minor version
data VersionConstraint = VExact      Version  
                       | VGt         Version
                       | VLt         Version
                       | VGte        Version
                       | VLte        Version
                       | VCompatible Version
 deriving (Eq,Show)

--- Compiler compatibility constraint, takes the name of the compiler (kics2 or
--- pakcs), as well as a disjunctive normal form combination of version 
--- constraints (see Dependency).
data CompilerCompatibility = CompilerCompatibility String Disjunction
 deriving (Eq,Show)

--- A package id consisting of the package name and version.
data PackageId = PackageId String Version

--- The specification to generate an executable from the package.
--- It consists of the name of the executable, the name of the main
--- module (which must contain an operation `main`), and list
--- of options for various compilers (i.e., pairs of compiler name and
--- options for this compiler).
data PackageExecutable = PackageExecutable String String [(String,String)]
 deriving (Eq,Show)

--- The specification of a single test suite for a package.
--- It consists of a directory, a list of modules, options (for CurryCheck),
--- and a name of a test script in this directory.
--- The test script can be empty, but then a non-empty list of modules must be
--- provided.
--- This structure specifies a test which is performed in the given directory
--- by running CurryCheck on the given list of modules where the option
--- string is passed to CurryCheck.
data PackageTest = PackageTest String [String] String String
 deriving (Eq,Show)

--- The specification to generate the documentation of the package.
--- It consists of the name of the directory containing the documentation,
--- a main file (usually, a LaTeX file) containing the documentation,
--- and a command to generate the documentation. If the command is missing
--- and the main file has the suffix "tex", e.g., "manual.tex",
--- the default command is "pdflatex manual.tex".
data PackageDocumentation = PackageDocumentation String String String
 deriving (Eq,Show)

--- A source where the contents of a package can be acquired.
--- @cons Http - URL to a ZIP file 
--- @cons Git - URL to a Git repository and an optional revision spec to check 
---   out
--- @cons FileSource - The path to a ZIP file to install. Cannot be specified in
---   a package specification file, for internal use only.
data PackageSource = Http String 
                   | Git String (Maybe GitRevision)
                   | FileSource String 
 deriving (Eq,Show)

--- A Git revision.
--- @cons Tag - A tag which might contain the string `$version$` which will
---             be replaced by the package version
--- @cons Ref - A Git 'commitish', i.e. a SHA, a branch name, a tag name etc.
--- @cons VersionAsTag - Use the package version prefixed with a 'v' as the tag
data GitRevision = Tag String
                 | Ref String
                 | VersionAsTag
 deriving (Eq,Show)

--- The data type for package specifications.
data Package = Package {
    name                  :: String
  , version               :: Version
  , author                :: [String]
  , maintainer            :: [String]
  , synopsis              :: String
  , description           :: Maybe String
  , category              :: [String]
  , license               :: Maybe String
  , licenseFile           :: Maybe String
  , copyright             :: Maybe String
  , homepage              :: Maybe String
  , bugReports            :: Maybe String
  , repository            :: Maybe String
  , dependencies          :: [Dependency]
  , compilerCompatibility :: [CompilerCompatibility]
  , source                :: Maybe PackageSource
  , sourceDirs            :: [String]
  , exportedModules       :: [String]
  , configModule          :: Maybe String
  , executableSpec        :: Maybe PackageExecutable
  , testSuite             :: Maybe [PackageTest]
  , documentation         :: Maybe PackageDocumentation
  }
 deriving (Eq,Show)

-- A simple Show instance for Package (maybe useful for debugging):
--instance Show Package where
--  show p = "(Package " ++
--           unwords [name p, showVersion (version p)
--                   , unwords (map showDependency (dependencies p))] ++ ")"

--- An empty package specification.
emptyPackage :: Package
emptyPackage = Package {
    name                  = ""
  , version               = initialVersion
  , author                = []
  , maintainer            = []
  , synopsis              = ""
  , description           = Nothing
  , category              = []
  , license               = Nothing
  , licenseFile           = Nothing
  , copyright             = Nothing
  , homepage              = Nothing
  , bugReports            = Nothing
  , repository            = Nothing
  , dependencies          = []
  , compilerCompatibility = []
  , source                = Nothing
  , sourceDirs            = []
  , exportedModules       = []
  , configModule          = Nothing
  , executableSpec        = Nothing
  , testSuite             = Nothing
  , documentation         = Nothing
  }

--- Returns the name of the executable of the package.
--- Returns the empty string if the package has no executable to install.
execOfPackage :: Package -> String
execOfPackage p =
  maybe "" (\ (PackageExecutable e _ _) -> e) (executableSpec p)

------------------------------------------------------------------------------
--- Translates a package to a JSON object.
packageSpecToJSON :: Package -> JValue
packageSpecToJSON pkg = JObject $
  [ ("name", JString $ name pkg)
  , ("version", JString $ showVersion $ version pkg) ] ++
  (case author pkg of []  -> [("author", JString "")]
                      [s] -> [("author", JString s)]
                      xs  -> stringListToJSON "author" xs) ++
  (case maintainer pkg of []  -> []
                          [s] -> [ ("maintainer", JString s) ]
                          xs  -> stringListToJSON "maintainer" xs) ++
  [ ("synopsis", JString $ synopsis pkg) ] ++
  maybeStringToJSON "description" (description pkg) ++
  stringListToJSON  "category"    (category pkg) ++
  maybeStringToJSON "license"     (license pkg) ++
  maybeStringToJSON "licenseFile" (licenseFile pkg) ++
  maybeStringToJSON "copyright"   (copyright   pkg) ++
  maybeStringToJSON "homepage"    (homepage    pkg) ++
  maybeStringToJSON "bugReports"  (bugReports  pkg) ++
  maybeStringToJSON "repository"  (repository  pkg) ++
  [ ("dependencies", dependenciesToJSON $ dependencies pkg) ] ++
  compilerCompatibilityToJSON (compilerCompatibility pkg) ++
  maybeSourceToJSON (source pkg) ++
  stringListToJSON "sourceDirs"      (sourceDirs pkg) ++
  stringListToJSON "exportedModules" (exportedModules pkg) ++
  maybeStringToJSON "configModule" (configModule pkg) ++
  maybeExecToJSON (executableSpec pkg) ++
  maybeTestToJSON (testSuite pkg) ++
  maybeDocuToJSON (documentation pkg)
 where
  dependenciesToJSON deps = JObject $ map dependencyToJSON deps
   where dependencyToJSON (Dependency p vc) =
           (p, JString $ showVersionConstraints vc)

  compilerCompatibilityToJSON deps =
    if null deps
      then []
      else [("compilerCompatibility", JObject $ map compatToJSON deps)]
   where compatToJSON (CompilerCompatibility p vc) =
           (p, JString $ showVersionConstraints vc)

  maybeSourceToJSON =
    maybe [] (\src -> [("source", JObject (pkgSourceToJSON src))])
   where
    pkgSourceToJSON (FileSource _) =
      error "Internal error: FileSource in package specification"
    pkgSourceToJSON (Http url) = [("http", JString url)]
    pkgSourceToJSON (Git url mbrev) =
      [("git", JString url)] ++ maybe [] revToJSON mbrev
     where
      revToJSON (Ref t)      = [("ref", JString t)]
      revToJSON (Tag t)      = [("tag", JString t)]
      revToJSON VersionAsTag = [("tag", JString "$version")]

  maybeExecToJSON =
    maybe [] (\ (PackageExecutable ename emain eopts) ->
                  [("executable", JObject $ [ ("name", JString ename)
                                            , ("main", JString emain)] ++
                                            exOptsToJSON eopts)])
   where
    exOptsToJSON eopts =
      if null eopts
        then []
        else [("options", JObject $ map (\ (c,o) -> (c, JString o)) eopts)]

  maybeTestToJSON = maybe [] (\tests -> [("testsuite", testsToJSON tests)])
   where
    testsToJSON tests = if length tests == 1
                          then testToJSON (head tests)
                          else JArray $ map testToJSON tests
    testToJSON (PackageTest dir mods opts script) = JObject $
      [ ("src-dir", JString dir) ] ++
      (if null opts then [] else [("options", JString opts)]) ++
      stringListToJSON "modules" mods ++
      (if null script then [] else [("script", JString script)])

  maybeDocuToJSON =
    maybe [] (\ (PackageDocumentation docdir docmain doccmd) ->
                  [("documentation",
                    JObject $ [ ("src-dir", JString docdir)
                              , ("main", JString docmain)] ++
                              if null doccmd
                                then []
                                else [("command", JString doccmd)] )])

  stringListToJSON fname exps =
    if null exps then []
                 else [(fname, JArray $ map JString exps)]

  maybeStringToJSON fname = maybe [] (\s -> [(fname, JString s)])


--- Writes a basic package specification to a JSON file.
---
--- @param pkg the package specification to write
--- @param file the file name to write to
writePackageSpec :: Package -> String -> IO ()
writePackageSpec pkg file = writeFile file $ ppJSON $ packageSpecToJSON pkg


--- Loads a package specification from a package directory.
---
--- @param the directory containing the package.json file
loadPackageSpec :: String -> IO (ErrorLogger Package)
loadPackageSpec dir = do
  let packageFile = dir </> "package.json"
  ifFileExists packageFile
    (do debugMessage $ "Reading package specification '" ++ packageFile ++ "'..."
        contents <- readCompleteFile packageFile
        case readPackageSpec contents of
          Left err -> failIO err
          Right v  -> succeedIO v )
    (failIO $ "Illegal package: file `package.json' does not exist!")

--- Checks whether two package ids are equal, i.e. if their names and versions
--- match.
---
--- @param p1 the first package
--- @param p2 the second package
packageIdEq :: Package -> Package -> Bool
packageIdEq p1 p2 = name p1 == name p2 && version p1 == version p2

--- Shows the package source in human-readable format.
showPackageSource :: Package -> String
showPackageSource pkg = case source pkg of
  Nothing -> "No source specified"
  Just  s -> showSource s
 where
  showSource (Git url rev)    = "Git " ++ url ++ showGitRev rev
  showSource (Http url)       = url
  showSource (FileSource url) = "File " ++ url

  showGitRev (Just (Ref ref)) = "@" ++ ref
  showGitRev (Just (Tag tag)) = "@" ++ replaceVersionInTag pkg tag
  showGitRev (Just VersionAsTag) = "@v" ++ (showVersion $ version pkg)
  showGitRev Nothing = ""

--- Replace the string `$version$` in a tag string by the current version.
replaceVersionInTag :: Package -> String -> String
replaceVersionInTag pkg =
  concat . intersperse (showVersion $ version pkg) . splitOn "$version$"

--- Less than operator for versions.
vlt :: Version -> Version -> Bool
vlt (majorA, minorA, patchA, preA) (majorB, minorB, patchB, preB) =
  major || minor || patch || pre
 where 
  major = majorA < majorB
  minor = majorA <= majorB && minorA < minorB
  patch = majorA <= majorB && minorA <= minorB && patchA < patchB
  pre   = case preA of
    Nothing -> case preB of
      Nothing -> patch
      Just  _ -> majorA <= majorB && minorA <= minorB && patchA <= patchB
    Just  a -> case preB of
      Nothing -> False
      Just b  -> a `ltPre` b

ltPre :: String -> String -> Bool
ltPre a b | isNumeric a && isNumeric b = readInt a < readInt b
          | isNumeric a = True
          | isNumeric b = False
          | otherwise   = a `ltShortlex` b

isNumeric :: String -> Bool
isNumeric = all isDigit

ltShortlex :: String -> String -> Bool
ltShortlex a b = (length a == length b && a < b) || length a < length b

test_shorterPrereleaseIsSmaller :: Prop
test_shorterPrereleaseIsSmaller =
  always $ (0, 0, 0, Just "rc") `vlt` (0, 0, 0, Just "beta")

test_numericIsSmallerLeft :: Prop
test_numericIsSmallerLeft =
  always $ (0, 0, 0, Just "1234") `vlt` (0, 0, 0, Just "rc")

test_numericIsSmallerRight :: Prop
test_numericIsSmallerRight =
  always $ not $ (0, 0, 0, Just "rc") `vlt` (0, 0, 0, Just "1234")

test_numbersAreComparedNumerically :: Prop
test_numbersAreComparedNumerically =
  always $ (0, 0, 0, Just "0003") `vlt` (0, 0, 0, Just "123")

--- Less than or equal operator for versions.
vlte :: Version -> Version -> Bool
vlte a b = a `vlt` b || a == b

--- Greater than operator for versions.
vgt :: Version -> Version -> Bool
vgt (majorA, minorA, patchA, preA) (majorB, minorB, patchB, preB) =
  major || minor || patch || pre
 where
  major = majorA > majorB
  minor = majorA >= majorB && minorA > minorB
  patch = majorA >= majorB && minorA >= minorB && patchA > patchB
  pre   = case preA of
    Nothing -> case preB of Nothing -> patch
                            Just  _ -> False
    Just a  -> case preB of Nothing -> False
                            Just b  -> a > b

--- Greater than or equal operator for versions.
vgte :: Version -> Version -> Bool
vgte a b = a `vgt` b || a == b

--- Is the version a pre-release version?
isPreRelease :: Version -> Bool
isPreRelease (_, _, _, Nothing) = False
isPreRelease (_, _, _, Just  _) = True

--- Gets the list of source directories of a package.
--- It is either the field `sourceDirs` (if non-empty) or `["src"]`.
sourceDirsOf :: Package -> [String]
sourceDirsOf p =
  if null (sourceDirs p) then ["src"]
                         else sourceDirs p

--- Gets the package names of all dependencies of a package.
dependencyNames :: Package -> [String]
dependencyNames p = map (\(Dependency s _) -> s) $ dependencies p

--- Renders a dependency as a string, including all version constraints.
showDependency :: Dependency -> String
showDependency (Dependency p vcs) = p ++ showVersionConstraints vcs

--- Renders a compiler dependency as a string, including all version
--- constraints.
showCompilerDependency :: CompilerCompatibility -> String
showCompilerDependency (CompilerCompatibility cc vcs) =
  cc ++ showVersionConstraints vcs

--- Renders a list of version constraints in disjunctive normal form.
showVersionConstraints :: [[VersionConstraint]] -> String
showVersionConstraints =
  intercalate " || " . map (intercalate ", " . map showVersionConstraint)

--- Renders a single version constraint as a string.
showVersionConstraint :: VersionConstraint -> String
showVersionConstraint (VLt v)         = " < "  ++ showVersion v
showVersionConstraint (VLte v)        = " <= " ++ showVersion v
showVersionConstraint (VGt v)         = " > "  ++ showVersion v
showVersionConstraint (VGte v)        = " >= " ++ showVersion v
showVersionConstraint (VExact v)      = " = "  ++ showVersion v
showVersionConstraint (VCompatible v) = " ~> " ++ showVersion v

--- Renders the id of a package as a string. Package name and version separated
--- by a dash.
packageId :: Package -> String
packageId p = name p ++ "-" ++ showVersion (version p)

--- Reads a package spec from a JSON string.
readPackageSpec :: String -> Either String Package
readPackageSpec s = case parseJSON s of
  Nothing -> Left "Invalid JSON"
  Just j -> case j of
    JObject kv -> packageSpecFromJObject kv
    _ -> Left "Expected a JSON object."

--- Reads a package spec from the key-value-pairs of a JObject.
packageSpecFromJObject :: [(String, JValue)] -> Either String Package
packageSpecFromJObject kv =
  mandatoryString "name" kv $ \name ->
  mandatoryString "version" kv $ \versionS ->
  getStringOrStringList True  "An author"    "author" $ \author ->
  getStringOrStringList False "A maintainer" "maintainer" $ \maintainer ->
  mandatoryString "synopsis" kv $ \synopsis ->
  optionalString "description" kv $ \description ->
  getStringList "A category" "category" $ \categories ->
  optionalString "license" kv $ \license ->
  optionalString "licenseFile" kv $ \licenseFile ->
  optionalString "copyright" kv $ \copyright ->
  optionalString "homepage" kv $ \homepage ->
  optionalString "bugReports" kv $ \bugReports ->
  optionalString "repository" kv $ \repository ->
  optionalString "configModule" kv $ \configModule ->
  mustBeVersion versionS $ \version ->
  getDependencies $ \dependencies ->
  getSource $ \source ->
  getStringList "A source directory" "sourceDirs" $ \sourcedirs ->
  getStringList "An exported module" "exportedModules" $ \exportedModules ->
  getCompilerCompatibility $ \compilerCompatibility ->
  getExecutableSpec $ \executable ->
  getTestSuite $ \testsuite ->
  getDocumentationSpec $ \docspec ->
  Right Package {
      name = name
    , version = version
    , author = author
    , maintainer = maintainer
    , synopsis = synopsis
    , description = description
    , category    = categories
    , license = license
    , licenseFile = licenseFile
    , copyright = copyright
    , homepage = homepage
    , bugReports = bugReports
    , repository = repository
    , dependencies = dependencies
    , compilerCompatibility = compilerCompatibility
    , source = source
    , sourceDirs      = sourcedirs
    , exportedModules = exportedModules
    , configModule    = configModule
    , executableSpec  = executable
    , testSuite       = testsuite
    , documentation   = docspec
    }
  where
    mustBeVersion :: String -> (Version -> Either String a) -> Either String a
    mustBeVersion s f = case readVersion s of
      Nothing -> Left $ "'" ++ s ++ "' is not a valid version specification."
      Just v -> f v
      
    getDependencies :: ([Dependency] -> Either String a) -> Either String a
    getDependencies f = case lookup "dependencies" kv of
      Nothing -> f []
      Just (JObject ds) -> case dependenciesFromJObject ds of
        Left e -> Left e
        Right ds' -> f ds'
      Just (JString _) -> Left $ "Expected an object, got a string" ++ forKey
      Just (JArray  _) -> Left $ "Expected an object, got an array" ++ forKey
      Just (JNumber _) -> Left $ "Expected an object, got a number" ++ forKey
      Just JTrue       -> Left $ "Expected an object, got 'true'"   ++ forKey
      Just JFalse      -> Left $ "Expected an object, got 'false'"  ++ forKey
      Just JNull       -> Left $ "Expected an object, got 'null'"   ++ forKey
     where forKey = " for key 'dependencies'"

    getCompilerCompatibility :: ([CompilerCompatibility] -> Either String a)
                             -> Either String a
    getCompilerCompatibility f = case lookup "compilerCompatibility" kv of
      Nothing -> f []
      Just (JObject ds) -> case compilerCompatibilityFromJObject ds of
        Left e -> Left e
        Right ds' -> f ds'
      Just (JString _) -> Left $ "Expected an object, got a string" ++ forKey
      Just (JArray  _) -> Left $ "Expected an object, got an array" ++ forKey
      Just (JNumber _) -> Left $ "Expected an object, got a number" ++ forKey
      Just JTrue       -> Left $ "Expected an object, got 'true'"   ++ forKey
      Just JFalse      -> Left $ "Expected an object, got 'false'"  ++ forKey
      Just JNull       -> Left $ "Expected an object, got 'null'"   ++ forKey
     where forKey = " for key 'compilerCompatibility'"

    getSource :: (Maybe PackageSource -> Either String a) -> Either String a
    getSource f = case lookup "source" kv of
      Nothing -> f Nothing
      Just (JObject s) -> case sourceFromJObject s of
        Left e -> Left e
        Right s' -> f (Just s')
      Just (JString _) -> Left $ "Expected an object, got a string" ++ forKey
      Just (JArray  _) -> Left $ "Expected an object, got an array" ++ forKey
      Just (JNumber _) -> Left $ "Expected an object, got a number" ++ forKey
      Just JTrue       -> Left $ "Expected an object, got 'true'"   ++ forKey
      Just JFalse      -> Left $ "Expected an object, got 'false'"  ++ forKey
      Just JNull       -> Left $ "Expected an object, got 'null'"   ++ forKey
     where forKey = " for key 'source'"

    getStringOrStringList :: Bool -> String -> String
                          -> ([String] -> Either String a)
                          -> Either String a
    getStringOrStringList mandatory keystr key f = case lookup key kv of
      Nothing -> if mandatory
                   then Left $ "Mandatory field missing: '" ++ key ++ "'"
                   else f []
      Just (JArray a)  -> case stringsFromJArray keystr a of
        Left  e -> Left e
        Right e -> f e
      Just (JString s) -> f [s]
      Just (JObject _) -> Left $ expectedText ++ "an object" ++ forKey
      Just (JNumber _) -> Left $ expectedText ++ "a number"  ++ forKey
      Just JTrue       -> Left $ expectedText ++ "'true'"    ++ forKey
      Just JFalse      -> Left $ expectedText ++ "'false'"   ++ forKey
      Just JNull       -> Left $ expectedText ++ "'null'"    ++ forKey
     where
      forKey       = " for key '" ++ key ++ "'"
      expectedText = "Expected an array, got "

    getStringList :: String -> String -> ([String] -> Either String a)
                  -> Either String a
    getStringList keystr key f = case lookup key kv of
      Nothing -> f []
      Just (JArray a)  -> case stringsFromJArray keystr a of
        Left  e -> Left e
        Right e -> f e
      Just (JObject _) -> Left $ expectedText ++ "an object" ++ forKey
      Just (JString _) -> Left $ expectedText ++ "a string"  ++ forKey
      Just (JNumber _) -> Left $ expectedText ++ "a number"  ++ forKey
      Just JTrue       -> Left $ expectedText ++ "'true'"    ++ forKey
      Just JFalse      -> Left $ expectedText ++ "'false'"   ++ forKey
      Just JNull       -> Left $ expectedText ++ "'null'"    ++ forKey
     where
      forKey       = " for key '" ++ key ++ "'"
      expectedText = "Expected an array, got "

    getExecutableSpec :: (Maybe PackageExecutable -> Either String a)
                      -> Either String a
    getExecutableSpec f = case lookup "executable" kv of
      Nothing -> f Nothing
      Just (JObject s) -> case execSpecFromJObject s of Left  e  -> Left e
                                                        Right s' -> f (Just s')
      Just (JString _) -> Left $ "Expected an object, got a string" ++ forKey
      Just (JArray  _) -> Left $ "Expected an object, got an array" ++ forKey
      Just (JNumber _) -> Left $ "Expected an object, got a number" ++ forKey
      Just JTrue       -> Left $ "Expected an object, got 'true'"   ++ forKey
      Just JFalse      -> Left $ "Expected an object, got 'false'"  ++ forKey
      Just JNull       -> Left $ "Expected an object, got 'null'"   ++ forKey
     where forKey = " for key 'executable'"

    getTestSuite :: (Maybe [PackageTest] -> Either String a) -> Either String a
    getTestSuite f = case lookup "testsuite" kv of
      Nothing          -> f Nothing
      Just (JObject s) -> case testSuiteFromJObject s of Left  e  -> Left e
                                                         Right s' -> f (Just s')
      Just (JArray  a) -> case testSuiteFromJArray a of
                            Left  e  -> Left e
                            Right s' -> f (Just s')
      Just (JString _) -> Left $ "Expected an object, got a string" ++ forKey
      Just (JNumber _) -> Left $ "Expected an object, got a number" ++ forKey
      Just JTrue       -> Left $ "Expected an object, got 'true'"   ++ forKey
      Just JFalse      -> Left $ "Expected an object, got 'false'"  ++ forKey
      Just JNull       -> Left $ "Expected an object, got 'null'"   ++ forKey
     where forKey = " for key 'testsuite'"

    getDocumentationSpec :: (Maybe PackageDocumentation -> Either String a)
                         -> Either String a
    getDocumentationSpec f = case lookup "documentation" kv of
      Nothing -> f Nothing
      Just (JObject s) -> case docuSpecFromJObject s of Left  e  -> Left e
                                                        Right s' -> f (Just s')
      Just (JString _) -> Left $ "Expected an object, got a string" ++ forKey
      Just (JArray  _) -> Left $ "Expected an object, got an array" ++ forKey
      Just (JNumber _) -> Left $ "Expected an object, got a number" ++ forKey
      Just JTrue       -> Left $ "Expected an object, got 'true'"   ++ forKey
      Just JFalse      -> Left $ "Expected an object, got 'false'"  ++ forKey
      Just JNull       -> Left $ "Expected an object, got 'null'"   ++ forKey
     where forKey = " for key 'documentation'"


mandatoryString :: String -> [(String, JValue)]
                -> (String -> Either String a) -> Either String a
mandatoryString k kv f = case lookup k kv of
  Nothing -> Left $ "Mandatory field missing: '" ++ k ++ "'"
  Just (JString s) -> f s
  Just (JObject _) -> Left $ "Expected a string, got an object" ++ forKey
  Just (JArray _)  -> Left $ "Expected a string, got an array" ++ forKey
  Just (JNumber _) -> Left $ "Expected a string, got a number" ++ forKey
  Just JTrue       -> Left $ "Expected a string, got 'true'" ++ forKey
  Just JFalse      -> Left $ "Expected a string, got 'false'" ++ forKey
  Just JNull       -> Left $ "Expected a string, got 'null'" ++ forKey
 where forKey = " for key '" ++ k ++ "'"
     
optionalString :: String -> [(String, JValue)]
               -> (Maybe String -> Either String a) -> Either String a
optionalString k kv f = case lookup k kv of
  Nothing -> f Nothing
  Just (JString s) -> f (Just s)
  Just (JObject _) -> Left $ "Expected a string, got an object" ++ forKey
  Just (JArray _)  -> Left $ "Expected a string, got an array" ++ forKey
  Just (JNumber _) -> Left $ "Expected a string, got a number" ++ forKey
  Just JTrue       -> Left $ "Expected a string, got 'true'" ++ forKey
  Just JFalse      -> Left $ "Expected a string, got 'false'" ++ forKey
  Just JNull       -> Left $ "Expected a string, got 'null'" ++ forKey
 where forKey = " for key '" ++ k ++ "'"

test_specFromJObject_mandatoryFields :: Prop
test_specFromJObject_mandatoryFields =
  is (packageSpecFromJObject obj)
     (\x -> isLeft x && isInfixOf "name" ((head . lefts) [x]))
  where obj = [("hello", JString "world")]

test_specFromJObject_invalidVersion :: Prop
test_specFromJObject_invalidVersion =
  is (packageSpecFromJObject obj)
     (\x -> isLeft x && isInfixOf "version" ((head . lefts) [x]))
 where obj = [ ("name", JString "mypackage"), ("author", JString "test")
             , ("synopsis", JString "great!"), ("version", JString "1.2.b")]

test_specFromJObject_minimalSpec :: Prop
test_specFromJObject_minimalSpec =
  is (packageSpecFromJObject obj) (\x -> isRight x && test x)
 where obj = [ ("name", JString "mypackage"), ("author", JString "me")
             , ("synopsis", JString "great!"), ("version", JString "1.2.3")]
       test x = author p == ["me"] && name p == "mypackage"
          where p = (head . rights) [x]

--- Reads a list of strings from a list of JValues.
stringsFromJArray :: String -> [JValue] -> Either String [String]
stringsFromJArray ekind a =
  if any isLeft strings
    then Left $ head $ lefts strings
    else Right $ rights strings
 where
  strings = map extractString a
  extractString s = case s of
    JString s' -> Right s'
    _          -> Left $ ekind ++ " must be a string"

--- Reads the dependency constraints of a package from the key-value-pairs of a
--- JObject.
dependenciesFromJObject :: [(String, JValue)] -> Either String [Dependency]
dependenciesFromJObject kv = if any isLeft dependencies
  then Left $ intercalate "; " (lefts dependencies)
  else Right $ rights dependencies
 where
  dependencies = map buildDependency kv
  buildDependency (pkg, JString vc) = case readVersionConstraints vc of
    Nothing -> Left $ "Invalid constraint '" ++ vc ++ "' for package '" ++
                       pkg ++ "'"
    Just v -> Right $ Dependency pkg v
  buildDependency (_,   JObject  _) = wrongVersionConstraint
  buildDependency (_,   JArray   _) = wrongVersionConstraint
  buildDependency (_,   JNumber  _) = wrongVersionConstraint
  buildDependency (_,   JTrue     ) = wrongVersionConstraint
  buildDependency (_,   JFalse    ) = wrongVersionConstraint
  buildDependency (_,   JNull     ) = wrongVersionConstraint

  wrongVersionConstraint = Left "Version constraint must be a string"

--- Reads the compiler compatibility constraints of a package from the 
--- key-value-pairs of a JObject.
compilerCompatibilityFromJObject :: [(String, JValue)]
                                 -> Either String [CompilerCompatibility]
compilerCompatibilityFromJObject kv = if any isLeft compilerCompats
  then Left $ intercalate "; " (lefts compilerCompats)
  else Right $ rights compilerCompats
 where
  compilerCompats = map buildCompilerCompat kv
  buildCompilerCompat (c, JString vc) = case readVersionConstraints vc of
    Nothing -> Left $ "Invalid constraint '" ++ vc ++ "' for compiler '" ++
                      c ++ "'"
    Just  v -> Right $ CompilerCompatibility c v
  buildCompilerCompat (_, JObject  _) = wrongVersionConstraint
  buildCompilerCompat (_, JArray   _) = wrongVersionConstraint
  buildCompilerCompat (_, JNumber  _) = wrongVersionConstraint
  buildCompilerCompat (_, JTrue     ) = wrongVersionConstraint
  buildCompilerCompat (_, JFalse    ) = wrongVersionConstraint
  buildCompilerCompat (_, JNull     ) = wrongVersionConstraint

  wrongVersionConstraint = Left "Version constraint must be a string"

--- Read source specification from the key-value-pairs of a JObject.
sourceFromJObject :: [(String, JValue)] -> Either String PackageSource
sourceFromJObject kv = case lookup "http" kv of
  Nothing -> case lookup "git" kv of
    Nothing -> Left $ "Only Git and HTTP supported"
    Just (JString url) -> case revisionFromJObject kv of
      Left err -> Left err
      Right rev -> Right $ Git url rev
    Just _ -> Left "Git expects url"
  Just (JString url) -> Right $ Http url
  Just _             -> Left "HTTP expects url"

--- Read Git revision specification from the key-value-pairs of a JObject.
revisionFromJObject :: [(String, JValue)] -> Either String (Maybe GitRevision)
revisionFromJObject kv = case lookup "tag" kv of
  Nothing -> case lookup "ref" kv of
    Nothing            -> Right Nothing
    Just (JString ref) -> Right $ Just $ Ref ref
    Just _             -> Left "Ref expects string"
  Just (JString tag) -> if tag == "$version"
    then Right $ Just $ VersionAsTag
    else Right $ Just $ Tag tag
  Just _             -> Left "Tag expects string"

--- Reads executable specification from the key-value-pairs of a JObject.
execSpecFromJObject :: [(String, JValue)] -> Either String PackageExecutable
execSpecFromJObject kv =
  mandatoryString "name"       kv $ \name ->
  optionalString  "main"       kv $ \main ->
  case lookup "options" kv of
    Nothing -> Right $ PackageExecutable name (maybe "Main" id main) []
    Just (JObject o) -> case optionsFromObject o of
      Left e -> Left e
      Right os -> Right $ PackageExecutable name (maybe "Main" id main) os
    Just _ -> Left "Expected an object for 'executable>options'"
 where
  optionsFromObject o =
    let os = map (extractString . snd) o
    in if any isLeft os
         then Left $ head (lefts os)
         else Right (zip (map fst o) (map fromRight os))
  
  extractString s = case s of
    JString s' -> Right s'
    _          -> Left $ "'executable>options': values must be strings"

--- Reads a test suite specification from the key-value-pairs of a JObject.
testSuiteFromJObject :: [(String, JValue)] -> Either String [PackageTest]
testSuiteFromJObject kv =
  mandatoryString "src-dir" kv $ \dir ->
  optionalString  "options" kv $ \opts ->
  optionalString  "script"  kv $ \scriptval ->
  let script = maybe "" id scriptval in
  case getOptStringList (not (null script)) "module" kv of
    Left e     -> Left e
    Right mods -> if null script && null mods
                    then Left emptyError
                    else if not (null script) && not (null mods)
                           then Left doubleError
                           else Right [PackageTest dir mods (maybe "" id opts)
                                                   script]
 where
  emptyError  = "'script' and 'modules' cannot be both empty in 'testsuite'"
  doubleError = "'script' and 'modules' cannot be both non-empty in 'testsuite'"

--- Reads the list of testsuites from a list of JValues (testsuite objects).
testSuiteFromJArray :: [JValue] -> Either String [PackageTest]
testSuiteFromJArray a =
  if any isLeft tests
    then Left $ head $ lefts tests
    else Right $ concat (rights tests)
 where
  tests = map extractTests a
  extractTests s = case s of
    JObject o -> testSuiteFromJObject o
    _         -> Left "Array element must be a testsuite object"

--- Reads an (optional, if first argument = True) key with a string list value.
getOptStringList :: Bool -> String -> [(String, JValue)]
                 -> Either String [String]
getOptStringList optional key kv = case lookup (key++"s") kv of
  Nothing -> if optional
               then Right []
               else Left $ "'"++key++"s' is not provided in 'testsuite'"
  Just (JArray a)  -> stringsFromJArray ("A "++key) a
  Just (JObject _) -> Left $ expectedText ++ "an object" ++ forKey
  Just (JString _) -> Left $ expectedText ++ "a string" ++ forKey
  Just (JNumber _) -> Left $ expectedText ++ "a number" ++ forKey
  Just JTrue       -> Left $ expectedText ++ "'true'" ++ forKey
  Just JFalse      -> Left $ expectedText ++ "'false'" ++ forKey
  Just JNull       -> Left $ expectedText ++ "'null'" ++ forKey
 where
  forKey = " for key '" ++ key ++ "s'"
  expectedText = "Expected an array, got "

--- Reads documentation specification from the key-value-pairs of a JObject.
docuSpecFromJObject :: [(String, JValue)] -> Either String PackageDocumentation
docuSpecFromJObject kv =
  mandatoryString "src-dir" kv $ \docdir ->
  mandatoryString "main"    kv $ \docmain ->
  optionalString  "command" kv $ \doccmd ->
  Right $ PackageDocumentation docdir docmain (maybe "" id doccmd)


--- Reads a dependency constraint expression in disjunctive normal form into 
--- a list of lists of version constraints. The inner lists are conjunctions of
--- version constraints, the outer list is a disjunction of conjunctions.
readVersionConstraints :: String -> Maybe [[VersionConstraint]]
readVersionConstraints s = parse pVersionConstraints (dropWhile isSpace s)

test_readVersionConstraints_single :: Prop
test_readVersionConstraints_single = readVersionConstraints "=1.2.3" -=- Just [[VExact (1, 2, 3, Nothing)]]

test_readVersionConstraints_multi :: Prop
test_readVersionConstraints_multi = readVersionConstraints "> 1.0.0, < 2.3.0" -=- Just [[VGt (1, 0, 0, Nothing), VLt (2, 3, 0, Nothing)]]

test_readVersionConstraints_disjunction :: Prop
test_readVersionConstraints_disjunction = readVersionConstraints ">= 4.0.0 || < 3.0.0, > 2.0.0" -=- Just [[VGte (4, 0, 0, Nothing)], [VLt (3, 0, 0, Nothing), VGt (2, 0, 0, Nothing)]]

pVersionConstraints :: Parser [[VersionConstraint]]
pVersionConstraints = (:) <$> pConjunction <*> (pWhitespace *> char '|' *> char '|' *> pWhitespace *> pVersionConstraints <|> yield [])

pConjunction :: Parser [VersionConstraint]
pConjunction = (:) <$> pVersionConstraint <*> (pWhitespace *> char ',' *> pWhitespace *> pConjunction <|> yield [])

--- Parses a version constraint.
readVersionConstraint :: String -> Maybe VersionConstraint
readVersionConstraint s = parse pVersionConstraint s

test_readVersionConstraint_exact :: Prop
test_readVersionConstraint_exact = readVersionConstraint "=1.2.3" -=- (Just $ VExact (1, 2, 3, Nothing))

test_readVersionConstraint_without :: Prop
test_readVersionConstraint_without = readVersionConstraint "1.2.3" -=- (Just $ VExact (1, 2, 3, Nothing))

test_readVersionConstraint_invalidVersion :: Prop
test_readVersionConstraint_invalidVersion = readVersionConstraint "=4.a.3" -=- Nothing

test_readVersionConstraint_invalidConstraint :: Prop
test_readVersionConstraint_invalidConstraint = readVersionConstraint "x1.2.3" -=- Nothing

test_readVersionConstraint_greaterThan :: Prop
test_readVersionConstraint_greaterThan = readVersionConstraint "> 1.2.3" -=- (Just $ VGt (1, 2, 3, Nothing))

test_readVersionConstraint_greaterThanEqual :: Prop
test_readVersionConstraint_greaterThanEqual = readVersionConstraint ">= 1.2.3" -=- (Just $ VGte (1, 2, 3, Nothing))

test_readVersionConstraint_lessThan :: Prop
test_readVersionConstraint_lessThan = readVersionConstraint "<1.2.3" -=- (Just $ VLt (1, 2, 3, Nothing))

test_readVersionConstraint_lessThanEqual :: Prop
test_readVersionConstraint_lessThanEqual = readVersionConstraint "<= 1.2.3" -=- (Just $ VLte (1, 2, 3, Nothing))

test_readVersionConstraint_compatible :: Prop
test_readVersionConstraint_compatible = readVersionConstraint "~>1.2.3" -=- (Just $ VCompatible (1, 2, 3, Nothing))

pVersionConstraint :: Parser VersionConstraint
pVersionConstraint = pConstraint <*> (pWhitespace *> pVersion)

pConstraint :: Parser (Version -> VersionConstraint)
pConstraint =   char '=' *> yield VExact
            <|> char '>' *> char '=' *> yield VGte
            <|> char '>' *> yield VGt
            <|> char '<' *> char '=' *> yield VLte
            <|> char '<' *> yield VLt
            <|> char '~' *> char '>' *> yield VCompatible
            <|> yield VExact

pWhitespace :: Parser ()
pWhitespace = some (char ' ') *> yield () <|> empty

--- Shows a version in dotted notation.
showVersion :: Version -> String
showVersion (maj, min, pat, pre) = majMinPat ++ preRelease
  where majMinPat = intercalate "." $ map show [maj, min, pat]
        preRelease = case pre of
          Just specifier -> "-" ++ specifier
          Nothing        -> ""

--- Tries to parse a version string.
tryReadVersion :: String -> IO (ErrorLogger Version)
tryReadVersion s = case readVersion s of
  Just v -> succeedIO v
  Nothing -> failIO $ s ++ " is not a valid version"

--- Tries to parse a version string.
readVersion :: String -> Maybe Version
readVersion s = parse pVersion s

pVersion :: Parser Version
pVersion =   pPureVersion
         <|> (\(maj, min, pat, _) pre -> (maj, min, pat, Just pre))
         <$> pPureVersion <*> (char '-' *> pPreRelease)

pPureVersion :: Parser Version
pPureVersion = (\maj (min, pat) -> (maj, min, pat, Nothing))
           <$> (pNum <* char '.') <*> ((\min pat -> (min, pat))
           <$> pNum <* char '.' <*> pNum)

pPreRelease :: Parser String
pPreRelease = some (check isAscii anyChar)

pNum :: Parser Int
pNum = (\cs -> foldl1 ((+).(10*)) (map (\c' -> ord c' - ord '0') cs))
   <$> some pDigit

pDigit :: Parser Char
pDigit = check isDigit anyChar
