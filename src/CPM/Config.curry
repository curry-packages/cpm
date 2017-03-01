--------------------------------------------------------------------------------
--- This module defines the data type for CPM's configuration options, the 
--- default values for all options, and functions for reading the user's .cpmrc
--- file and merging its contents into the default options.
--------------------------------------------------------------------------------

module CPM.Config 
  ( Config ( Config, packageInstallDir, binInstallDir, repositoryDir
           , packageIndexRepository )
  , readConfiguration, defaultConfig) where

import Char         (isSpace)
import Directory    (doesFileExist, getHomeDirectory, createDirectoryIfMissing)
import FilePath     ((</>))
import Function     ((***))
import List         (splitOn, intersperse)
import Maybe        (mapMaybe)
import PropertyFile (readPropertyFile)

import CPM.ErrorLogger

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
    --- URL to the package index repository
  , packageIndexRepository :: String
  }

--- CPM's default configuration values. These are used if no .cpmrc file is found
--- or a new value for the option is not specified in the .cpmrc file.
defaultConfig :: Config
defaultConfig = Config
  { packageInstallDir = "$HOME/.cpm/packages"
  , binInstallDir = "$HOME/.cpm/bin"
  , repositoryDir = "$HOME/.cpm/index" 
  , packageIndexRepository = packageIndexURI }

--- Reads the .cpmrc file from the user's home directory (if present) and merges
--- its contents into the default configuration. Resolves the $HOME variable 
--- after merging and creates any missing directories. May return an error using 
--- Left.
readConfiguration :: IO (Either String Config)
readConfiguration = do
  home <- getHomeDirectory
  configFile <- return $ home </> ".cpmrc"
  exists <- doesFileExist configFile
  settingsFromFile <- if exists
    then readPropertyFile configFile >>= \p -> return $ stripProps p
    else return []
  mergedSettings <- return $ mergeConfigFile defaultConfig settingsFromFile
  case mergedSettings of
    Left e   -> return $ Left e
    Right s' -> replaceHome s' >>= \s'' -> createDirectories s'' >>
                return (Right s'')

replaceHome :: Config -> IO Config
replaceHome cfg = do
  homeDir <- getHomeDirectory
  return $ cfg {
      packageInstallDir = replaceHome' homeDir (packageInstallDir cfg)
    , binInstallDir     = replaceHome' homeDir (binInstallDir cfg)
    , repositoryDir     = replaceHome' homeDir (repositoryDir cfg)
  }
 where
  replaceHome' h s = concat $ intersperse h $ splitOn "$HOME" s

createDirectories :: Config -> IO ()
createDirectories cfg = do
  createDirectoryIfMissing True (packageInstallDir cfg)
  createDirectoryIfMissing True (binInstallDir cfg)
  createDirectoryIfMissing True (repositoryDir cfg)

--- Merges configuration options from a configuration file into a configuration
--- record. May return an error using Left.
---
--- @param cfg - the configuration record to merge into
--- @param opts - the options to merge
mergeConfigFile :: Config -> [(String, String)] -> Either String Config
mergeConfigFile cfg props = applyEither setters cfg
 where
  setters = mapMaybe id $ map maybeApply props
  maybeApply (k, v) = case lookup k keySetters of
    Nothing -> Nothing
    Just  s -> Just $ s v

--- Removes leading and trailing whitespace from option keys and values.
---
--- @param opts - the options
stripProps :: [(String, String)] -> [(String, String)]
stripProps = map (strip *** strip) 
 where
  strip s = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace s

--- A map from option names to functions that will update a configuration
--- record with a value for that option.
keySetters :: [(String, String -> Config -> Either String Config)]
keySetters =
  [ ("repository_path"     , \v c -> Right $ c { repositoryDir     = v })
  , ("package_install_path", \v c -> Right $ c { packageInstallDir = v})
  , ("bin_install_path"    , \v c -> Right $ c { binInstallDir     = v})
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
