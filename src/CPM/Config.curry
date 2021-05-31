------------------------------------------------------------------------------
--- This module defines the data type for CPM's configuration options, the
--- default values for all options, and functions for reading the user's .cpmrc
--- file and merging its contents into the default options.
------------------------------------------------------------------------------

module CPM.Config
  ( Config ( Config, packageInstallDir, binInstallDir, repositoryDir
           , appPackageDir, packageIndexURLs, packageTarFilesURLs
           , homePackageDir, curryExec
           , compilerVersion, compilerBaseVersion )
  , readConfigurationWith, defaultConfig
  , showConfiguration, showCompilerVersion ) where

import Control.Monad     ( unless )
import Data.Char         ( toUpper )
import System.Directory  ( doesDirectoryExist, createDirectoryIfMissing
                         , getHomeDirectory, doesFileExist )
import qualified Curry.Compiler.Distribution as Dist
import System.FilePath   ( (</>), isAbsolute )
import Data.Maybe        ( mapMaybe )
import Data.List         ( split, splitOn, intercalate, intersperse )
import Control.Monad     ( when )
import System.IOExts     ( evalCmd )

import Data.PropertyFile ( readPropertyFile )
import System.Path       ( getFileInPath )

import CPM.ErrorLogger
import CPM.FileUtil ( ifFileExists )
import CPM.Helpers  ( strip )

--- The default URL prefix to the directory containing tar files of all packages
packageTarFilesDefaultURLs :: [String]
packageTarFilesDefaultURLs =
  ["https://www-ps.informatik.uni-kiel.de/~cpm/PACKAGES"]

--- The default location of the central package index.
packageIndexDefaultURLs :: [String]
packageIndexDefaultURLs =
  map  (++"/INDEX.tar.gz") packageTarFilesDefaultURLs ++
  ["https://git.ps.informatik.uni-kiel.de/curry-packages/cpm-index.git"]
-- If you have an ssh access to git.ps.informatik.uni-kiel.de:
--["ssh://git@git.ps.informatik.uni-kiel.de:55055/curry-packages/cpm-index.git"]

--- Data type containing the main configuration of CPM.
data Config = Config {
    --- The directory where locally installed packages are stored
    packageInstallDir :: String
    --- The directory where executable of locally installed packages are stored
  , binInstallDir :: String
    --- Directory where the package repository is stored
  , repositoryDir :: String
    --- Directory where the application packages are stored (cmd 'install')
  , appPackageDir :: String
    --- URLs tried for downloading the package index
  , packageIndexURLs :: [String]
    --- URL prefixes to the directory containing tar files of all packages
  , packageTarFilesURLs :: [String]
    --- The directory where the default home package is stored
  , homePackageDir :: String
    --- The executable of the Curry system used to compile and check packages
  , curryExec :: String
    --- The compiler version (name,major,minor,rev) used to compile packages
  , compilerVersion :: (String,Int,Int,Int)
    --- The version of the base libraries used by the compiler
  , compilerBaseVersion :: String
  }

--- CPM's default configuration values. These are used if no .cpmrc file is found
--- or a new value for the option is not specified in the .cpmrc file.
defaultConfig :: Config
defaultConfig = Config
  { packageInstallDir      = "$HOME/.cpm/packages"
  , binInstallDir          = "$HOME/.cpm/bin"
  , repositoryDir          = "$HOME/.cpm/index" 
  , appPackageDir          = ""
  , packageIndexURLs       = packageIndexDefaultURLs
  , packageTarFilesURLs    = packageTarFilesDefaultURLs
  , homePackageDir         = ""
  , curryExec              = Dist.installDir </> "bin" </> Dist.curryCompiler
  , compilerVersion        = ( Dist.curryCompiler
                             , Dist.curryCompilerMajorVersion
                             , Dist.curryCompilerMinorVersion
                             , Dist.curryCompilerRevisionVersion )
  , compilerBaseVersion    = Dist.baseVersion
  }

--- Shows the configuration.
showConfiguration :: Config -> String
showConfiguration cfg = unlines
  [ "Compiler version       : " ++ showCompilerVersion cfg
  , "Compiler base version  : " ++ compilerBaseVersion cfg
  , "CURRY_BIN              : " ++ curryExec           cfg
  , "REPOSITORY_PATH        : " ++ repositoryDir       cfg
  , "PACKAGE_INSTALL_PATH   : " ++ packageInstallDir   cfg
  , "BIN_INSTALL_PATH       : " ++ binInstallDir       cfg
  , "APP_PACKAGE_PATH       : " ++ appPackageDir       cfg
  , "HOME_PACKAGE_PATH      : " ++ homePackageDir      cfg
  , "PACKAGE_INDEX_URL      : " ++ intercalate "|" (packageIndexURLs cfg)
  , "PACKAGE_TARFILES_URL   : " ++ intercalate "|" (packageTarFilesURLs cfg)
  ]

--- Shows the compiler version in the configuration.
showCompilerVersion :: Config -> String
showCompilerVersion cfg =
  let (cname,cmaj,cmin,crev) = compilerVersion cfg
  in cname ++ ' ' : showVersionNumber (cmaj,cmin,crev)

--- Shows a version consisting of major/minor,revision number.
showVersionNumber :: (Int,Int,Int) -> String
showVersionNumber (maj,min,rev) =
  show maj ++ "." ++ show min ++ "." ++ show rev

--- Sets an existing compiler executable in the configuration.
--- Try to use the predefined CURRYBIN value.
--- If it is an absolute path name but does not exists,
--- try to find the executable "curry" in the path.
setCompilerExecutable :: Config -> ErrorLogger Config
setCompilerExecutable cfg = do
  let exec = curryExec cfg
  if isAbsolute exec
    then do isexec <- liftIOEL $ doesFileExist exec
            if isexec
              then return cfg
              else do
                logInfo $ "Warning: executable '" ++ exec ++ "' not found!"
                logInfo $ "Looking for 'curry' in path..."
                findExecutable "curry"
    else findExecutable exec
 where
  findExecutable exec = liftIOEL $
    getFileInPath exec >>=
    maybe (error $ "Executable '" ++ exec ++ "' not found in path!")
          (\absexec -> return cfg { curryExec = absexec })

--- Sets the `appPackageDir` depending on the compiler version.
setAppPackageDir :: Config -> IO Config
setAppPackageDir cfg
  | null (appPackageDir cfg)
  = do homedir <- getHomeDirectory
       let cpmdir = homedir </> ".cpm"
           (cname,cmaj,cmin,crev) = compilerVersion cfg
           cmpname = cname ++ "_" ++ showVersionNumber (cmaj,cmin,crev)
       return cfg { appPackageDir = cpmdir </> "apps_" ++ cmpname }
  | otherwise = return cfg

--- Sets the `homePackageDir` depending on the compiler version.
setHomePackageDir :: Config -> IO Config
setHomePackageDir cfg
  | null (homePackageDir cfg)
  = do homedir <- getHomeDirectory
       let cpmdir = homedir </> ".cpm"
           (cname,cmaj,cmin,crev) = compilerVersion cfg
           cvname     = cname ++ "-" ++ showVersionNumber (cmaj,cmin,crev)
           homepkgdir = cpmdir </> cvname ++ "-homepackage"
       return cfg { homePackageDir = homepkgdir }
  | otherwise = return cfg

--- Sets the correct compiler version in the configuration.
setCompilerVersion :: Config -> ErrorLogger Config
setCompilerVersion cfg0 = do
  cfg <- setCompilerExecutable cfg0
  if curryExec cfg == Dist.installDir </> "bin" </> Dist.curryCompiler
    then return cfg { compilerVersion = currVersion
                    , compilerBaseVersion = Dist.baseVersion }
    else do (sname,svers,sbver) <- getCompilerVersion (curryExec cfg)
            let cname = strip sname
                cvers = strip svers
                bvers = strip sbver
                (majs:mins:revs:_) = split (=='.') cvers
            logDebug $ unwords ["Compiler version:",cname,cvers]
            logDebug $ "Base lib version: " ++ bvers
            return cfg { compilerVersion = (cname, read majs,
                                            read mins, read revs)
                       , compilerBaseVersion = bvers }
 where
  getCompilerVersion currybin = do
    logDebug $ "Getting version information from " ++ currybin
    (r,s,e) <-  liftIOEL $ evalCmd currybin
                 ["--compiler-name","--numeric-version","--base-version"] ""
    if r>0
      then error $ "Cannot determine compiler version:\n" ++ e
      else case lines s of
        [sname,svers,sbver] -> return (sname,svers,sbver)
        _ -> do logDebug $ "Query version information again..."
                (c1,sname,e1) <- liftIOEL $ evalCmd currybin ["--compiler-name"] ""
                (c2,svers,e2) <- liftIOEL $ evalCmd currybin ["--numeric-version"] ""
                (c3,sbver,e3) <- liftIOEL $ evalCmd currybin ["--base-version"] ""
                when (c1 > 0 || c2 > 0 || c3 > 0) $
                  error $ "Cannot determine compiler version:\n" ++
                          unlines (filter (not . null) [e1,e2,e3])
                return (sname,svers,sbver)

  currVersion = (Dist.curryCompiler, Dist.curryCompilerMajorVersion,
                                     Dist.curryCompilerMinorVersion
                                   , Dist.curryCompilerRevisionVersion)

--- Reads the .cpmrc file from the user's home directory (if present) and
--- merges its contents and some given default settings (first argument)
--- into the configuration used by CPM.
--- Resolves the $HOME variable after merging and creates
--- any missing directories. May return an error using `Left`.
readConfigurationWith :: [(String,String)] -> ErrorLogger (Either String Config)
readConfigurationWith defsettings = do
  home <- liftIOEL $ getHomeDirectory
  let configFile = home </> ".cpmrc"
  exfile <- liftIOEL $ doesFileExist configFile
  rcsettings <- liftIOEL $
    if exfile
      then do rcdefs <- readPropertyFile configFile >>= return . stripProps
              return rcdefs
      else return []
  unless (null rcsettings) $ logDebug $
    "Properties defined in " ++ configFile ++ ":\n" ++
    unlines (map (\ (x,y) -> "  " ++ x ++ "=" ++ y) rcsettings)
  let mergedSettings = mergeConfigSettings defaultConfig
                         (rcsettings ++ stripProps defsettings)
  case mergedSettings of
    Left e   -> return $ Left e
    Right s0 -> do s1 <- liftIOEL $ replaceHome s0
                   s2 <- setCompilerVersion s1
                   s3 <- liftIOEL $ setAppPackageDir   s2
                   s4 <- liftIOEL $ setHomePackageDir  s3
                   liftIOEL $ createDirectories s4
                   return $ Right s4

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
stripProps = map (\(a,b) -> ((map toUpper $ filter (/='_') $ strip a), strip b))

--- A map from option names to functions that will update a configuration
--- record with a value for that option.
keySetters :: [(String, String -> Config -> Config)]
keySetters =
  [ ("APPPACKAGEPATH"     , \v c -> c { appPackageDir       = v })
  , ("BININSTALLPATH"     , \v c -> c { binInstallDir       = v })
  , ("CURRYBIN"           , \v c -> c { curryExec           = v })
  , ("HOMEPACKAGEPATH"    , \v c -> c { homePackageDir      = v })
  , ("PACKAGEINDEXURL"    , \v c -> c { packageIndexURLs    = breakURLs v })
  , ("PACKAGETARFILESURL" , \v c -> c { packageTarFilesURLs = breakURLs v })
  , ("PACKAGEINSTALLPATH" , \v c -> c { packageInstallDir   = v })
  , ("REPOSITORYPATH"     , \v c -> c { repositoryDir       = v })
  ]
 where
  breakURLs = splitOn "|"

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
