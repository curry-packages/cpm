------------------------------------------------------------------------------
--- Some auxiliary operations to deal with executables (system commands)
--- used by CPM.
------------------------------------------------------------------------------

module CPM.Executables
  ( checkRequiredExecutables, getCurlCmd, getCurlCmdOpts
  , getCurryCheck, getCurryDoc )
 where

import Control.Monad       ( unless )
import Data.List           ( intercalate )

import System.Directory    ( doesFileExist )
import System.FilePath     ( (</>) )
import System.Path         ( fileInPath, getFileInPath )
import System.Process      ( exitWith )

import CPM.Config          ( Config, binInstallDir )
import CPM.ErrorLogger

------------------------------------------------------------------------------
--- Check whether all operating system executables required by CPM are present
--- on the current system.
--- Since this takes some time, it is only checked with CPM's `update` command.
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
    , "readlink"
    , "realpath" ]

--- Filters from a given list of executable names the executables
--- that cannot be found in one of the directories of the environment
--- variable `PATH`. 
checkExecutables :: [String] -> IO [String]
checkExecutables executables = do
  present <- mapM fileInPath executables
  return $ map fst $ filter (not . snd) (zip executables present)

------------------------------------------------------------------------------
--- Returns the `curl` command (first component of the result)
--- together with some standard options. If the log level is not `Debug`,
--- the options `--silent --show-error` are added so that
--- `curl` works in silent mode. Moreover, a max-time is used to avoid
--- hanging forever if a server cannot be reached.
getCurlCmdOpts :: ErrorLogger (String,[String])
getCurlCmdOpts = do
  ll <- getLogLevel
  return $ ("curl",
            ["--max-time", "30"] ++
            (if ll == Debug then [] else ["--silent", "--show-error"]))

--- Returns the `curl` command with some standard options as a string.
--- If the log level is not `Debug`, the options `--silent --show-error`
--- are added so that `curl` works in silent mode. Moreover, a max-time
--- is used to avoid hanging forever if a server cannot be reached.
getCurlCmd :: ErrorLogger String
getCurlCmd = fmap (\(c,os) -> unwords (c:os)) getCurlCmdOpts

------------------------------------------------------------------------------
--- Returns the `curry-check` command, either from the current path
--- or from CPM's bin directory, or `Nothing` if it does not exist.
--- If it does not exist, report this also as an info.
getCurryCheck :: Config -> ErrorLogger (Maybe String)
getCurryCheck cfg = do
  mbf <- liftIOEL $ getFileInPath ccbin
  maybe (do let cpmcurrycheck = binInstallDir cfg </> ccbin
            ccex <- liftIOEL $ doesFileExist cpmcurrycheck
            if ccex then return $ Just cpmcurrycheck
                    else do logInfo "Executable 'curry-check' not found!"
                            return Nothing
        )
        (return . Just)
        mbf
 where
  ccbin = "curry-check"

------------------------------------------------------------------------------
--- Returns the `curry-doc` command, either from the current path
--- or from CPM's bin directory. Fails with an error if it does not exist.
getCurryDoc :: Config -> String -> ErrorLogger String
getCurryDoc cfg cdbin = do
  mbf <- liftIOEL $ getFileInPath cdbin
  maybe (do let cpmcurrydoc = binInstallDir cfg </> cdbin
            cdex <- liftIOEL $ doesFileExist cpmcurrydoc
            if cdex then return cpmcurrydoc
                    else fail $ "Executable '" ++ cdbin ++ "' not found!"
        )
        return
        mbf

------------------------------------------------------------------------------
