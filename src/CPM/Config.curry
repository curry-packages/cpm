------------------------------------------------------------------------------
--- This module defines the data type for CPM's configuration options, the 
--- default values for all options, and functions for reading the user's .cpmrc
--- file and merging its contents into the default options.
------------------------------------------------------------------------------

module CPM.Config 
  ( Config ( Config, packageInstallDir, binInstallDir, repositoryDir
           , appPackageDir, packageIndexRepository, curryExec
           , compilerVersion, baseVersion )
  , readConfigurationWith, defaultConfig
  , showConfiguration, showCompilerVersion ) where

import Char         ( toUpper )
import Directory    ( getHomeDirectory, createDirectoryIfMissing )
import qualified Distribution as Dist
import FilePath     ( (</>), isAbsolute )
import Function     ( (***) )
import IOExts       ( evalCmd )
import List         ( split, splitOn, intersperse )
import Maybe        ( mapMaybe )
import PropertyFile ( readPropertyFile )
import Read         ( readInt )

import CPM.ErrorLogger
import CPM.FileUtil ( ifFileExists, getFileInPath )
import CPM.Helpers  ( strip )

--- The location of the central package index.
packageIndexURI :: String
packageIndexURI =
  "https://git.ps.informatik.uni-kiel.de/curry-packages/cpm-index.git"
  -- if you have an ssh access to git.ps.informatik.uni-kiel.de:
  -- "ssh://git@git.ps.informatik.uni-kiel.de:55055/curry-packages/cpm-index.git"

--- Data type containing the main configuration of CPM.
data Config = Config {
    --- The directory where locally installed packages are stored
    packageInstallDir :: String
    --- The directory where executable of locally installed packages are stored
  , binInstallDir :: String
    --- Directory where the package repository is stored
  , repositoryDir :: String
    --- Directory where the application packages are stored (cmd 'installapp')
  , appPackageDir :: String
    --- URL to the package index repository
  , packageIndexRepository :: String
    --- The executable of the Curry system used to compile and check packages
  , curryExec :: String
    --- The compiler version (name,major,minor) used to compile packages
  , compilerVersion :: (String,Int,Int)
    --- The version of the base libraries used by the compiler
  , baseVersion :: String
  }

--- CPM's default configuration values. These are used if no .cpmrc file is found
--- or a new value for the option is not specified in the .cpmrc file.
defaultConfig :: Config
defaultConfig = Config
  { packageInstallDir      = "$HOME/.cpm/packages"
  , binInstallDir          = "$HOME/.cpm/bin"
  , repositoryDir          = "$HOME/.cpm/index" 
  , appPackageDir          = "$HOME/.cpm/app_packages" 
  , packageIndexRepository = packageIndexURI
  , curryExec              = Dist.installDir </> "bin" </> Dist.curryCompiler
  , compilerVersion        = ( Dist.curryCompiler
                             , Dist.curryCompilerMajorVersion
                             , Dist.curryCompilerMinorVersion )
  , baseVersion            = Dist.baseVersion
  }

--- Shows the configuration.
showConfiguration :: Config -> String
showConfiguration cfg = unlines
  [ "Current configuration:"
  , "Compiler version  : " ++ showCompilerVersion cfg
  , "Base version      : " ++ baseVersion         cfg
  , "CURRYBIN          : " ++ curryExec           cfg
  , "REPOSITORYPATH    : " ++ repositoryDir       cfg
  , "PACKAGEINSTALLPATH: " ++ packageInstallDir   cfg
  , "BININSTALLPATH    : " ++ binInstallDir       cfg
  , "APPPACKAGEPATH    : " ++ appPackageDir       cfg
  ]
  
--- Shows the compiler version in the configuration.
showCompilerVersion :: Config -> String
showCompilerVersion cfg =
  let (cname,cmaj,cmin) = compilerVersion cfg
  in cname ++ ' ' : show cmaj ++ "." ++ show cmin

--- Sets an existing compiler executable in the configuration.
--- Try to use the predefined CURRYBIN value.
--- If it is an absolute path name but does not exists,
--- try to find the executable "curry" in the path.
setCompilerExecutable :: Config -> IO Config
setCompilerExecutable cfg = do
  let exec = curryExec cfg
  if isAbsolute exec
    then ifFileExists exec (return cfg) (findExecutable "curry")
    else findExecutable exec
 where
  findExecutable exec =
    getFileInPath exec >>=
    maybe (error $ "Executable '" ++ exec ++ "' not found in path!")
          (\absexec -> return cfg { curryExec = absexec })

--- Sets the correct compiler version in the configuration.
setCompilerVersion :: Config -> IO Config
setCompilerVersion cfg0 = do
  cfg <- setCompilerExecutable cfg0
  if curryExec cfg == Dist.installDir </> "bin" </> Dist.curryCompiler
    then return cfg { compilerVersion = currVersion
                    , baseVersion = Dist.baseVersion }
    else do (c1,sname,e1) <- evalCmd (curryExec cfg) ["--compiler-name"] ""
            (c2,svers,e2) <- evalCmd (curryExec cfg) ["--numeric-version"] ""
            (c3,sbver,e3) <- evalCmd (curryExec cfg) ["--base-version"] ""
            when (c1 > 0 || c2 > 0 || c3 > 0) $
              error $ "Cannot determine compiler version:\n" ++
                      unlines (filter (not . null) [e1,e2,e3])
            let cname = strip sname
                cvers = strip svers
                bvers = strip sbver
                (majs:mins:_) = split (=='.') cvers
            debugMessage $ "Compiler version: " ++ cname ++ " " ++ cvers
            debugMessage $ "Base lib version: " ++ bvers
            return cfg { compilerVersion = (cname, readInt majs, readInt mins)
                       , baseVersion = bvers }
 where
  currVersion = (Dist.curryCompiler, Dist.curryCompilerMajorVersion,
                                     Dist.curryCompilerMinorVersion)

--- Reads the .cpmrc file from the user's home directory (if present) and
--- merges its contents and some given default settings (first argument)
--- into the configuration used by CPM.
--- Resolves the $HOME variable after merging and creates
--- any missing directories. May return an error using `Left`.
readConfigurationWith :: [(String,String)] -> IO (Either String Config)
readConfigurationWith defsettings = do
  home <- getHomeDirectory
  configFile <- return $ home </> ".cpmrc"
  settingsFromFile <-
    ifFileExists configFile
                 (readPropertyFile configFile >>= return . stripProps)
                 (return [])
  let mergedSettings = mergeConfigSettings defaultConfig
                         (settingsFromFile ++ stripProps defsettings)
  case mergedSettings of
    Left e   -> return $ Left e
    Right s0 -> do s1 <- replaceHome s0
                   createDirectories s1
                   s2 <- setCompilerVersion s1
                   return $ Right s2

replaceHome :: Config -> IO Config
replaceHome cfg = do
  homeDir <- getHomeDirectory
  return $ cfg {
      packageInstallDir = replaceHome' homeDir (packageInstallDir cfg)
    , binInstallDir     = replaceHome' homeDir (binInstallDir cfg)
    , repositoryDir     = replaceHome' homeDir (repositoryDir cfg)
    , appPackageDir     = replaceHome' homeDir (appPackageDir cfg)
  }
 where
  replaceHome' h s = concat $ intersperse h $ splitOn "$HOME" s

createDirectories :: Config -> IO ()
createDirectories cfg = do
  createDirectoryIfMissing True (packageInstallDir cfg)
  createDirectoryIfMissing True (binInstallDir cfg)
  createDirectoryIfMissing True (repositoryDir cfg)
  createDirectoryIfMissing True (appPackageDir cfg)

--- Merges configuration options from a configuration file or argument options
--- into a configuration record. May return an error using Left.
---
--- @param cfg - the configuration record to merge into
--- @param opts - the options to merge
mergeConfigSettings :: Config -> [(String, String)] -> Either String Config
mergeConfigSettings cfg props = applyEither setters cfg
 where
  setters = map maybeApply props
  maybeApply (k, v) = case lookup k keySetters of
    Nothing -> \_ -> Left $ "Unknown .cpmrc property: " ++ k ++ "\n\n" ++
                            "The following .cpmrc properties are allowed:\n" ++
                            unlines (map fst keySetters)
    Just  s -> \c -> Right $ s v c

--- Removes leading and trailing whitespaces from option keys and values
--- and transforms option keys to uppercase where underscores are removed.
---
--- @param opts - the options
stripProps :: [(String, String)] -> [(String, String)]
stripProps = map ((map toUpper . filter (/='_') . strip) *** strip) 

--- A map from option names to functions that will update a configuration
--- record with a value for that option.
keySetters :: [(String, String -> Config -> Config)]
keySetters =
  [ ("REPOSITORYPATH"     , \v c -> c { repositoryDir     = v })
  , ("PACKAGEINSTALLPATH" , \v c -> c { packageInstallDir = v })
  , ("BININSTALLPATH"     , \v c -> c { binInstallDir     = v })
  , ("APPPACKAGEPATH"     , \v c -> c { appPackageDir     = v })
  , ("CURRYBIN"           , \v c -> c { curryExec         = v })
  ]

--- Sequentially applies a list of functions that transform a value to a value
--- of that type (i.e. a fold). Each function can error out with a Left, in 
--- which case no further applications are done and the Left is returned from
--- the overall application of applyEither.
---
--- @param fs - the list of functions
--- @param v - the initial value
applyEither :: [a -> Either c a] -> a -> Either c a
applyEither [] z = Right z
applyEither (f:fs) z = case f z of
  Left err -> Left err
  Right z' -> applyEither fs z'

------------------------------------------------------------------------------
