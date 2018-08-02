--------------------------------------------------------------------------------
--- Some utilities for deailing with files and directories
--- for the Curry Package Manager.
--------------------------------------------------------------------------------

module CPM.FileUtil
  ( joinSearchPath
  , copyDirectory
  , createSymlink
  , removeSymlink
  , isSymlink
  , linkTarget
  , copyDirectoryFollowingSymlinks
  , quote
  , fileInPath, getFileInPath
  , tempDir
  , inTempDir
  , inDirectory
  , recreateDirectory
  , removeDirectoryComplete
  , safeReadFile, checkAndGetVisibleDirectoryContents
  , whenFileExists, ifFileExists
  ) where

import System.Directory   ( doesFileExist, doesDirectoryExist
                          , setCurrentDirectory, getDirectoryContents
                          , getTemporaryDirectory, doesDirectoryExist
                          , createDirectory, getCurrentDirectory
                          , createDirectoryIfMissing, getAbsolutePath )
import System.Process     ( system, exitWith )
import System.Environment ( getEnv )
import IOExts             ( evalCmd, readCompleteFile )
import System.FilePath    ( FilePath, replaceFileName, (</>)
                          , searchPathSeparator )
import Data.List          ( intercalate, isPrefixOf, splitOn )

--- Joins a list of directories into a search path.
joinSearchPath :: [FilePath] -> String
joinSearchPath = intercalate [searchPathSeparator] . map emptyPath2Dot
 where
  emptyPath2Dot p = if null p then "." else p

--- Recursively copies a directory structure.
copyDirectory :: String -> String -> IO ()
copyDirectory src dst = do
  retCode <- system $ "cp -pR \"" ++ src ++ "\" \"" ++ dst ++ "\""
  if retCode /= 0
    then error $ "Copy failed with " ++ (show retCode)
    else return ()

--- Recursively copies a directory structure following symlinks, i.e. links
--- get replaced by copies in the destination.
copyDirectoryFollowingSymlinks :: String -> String -> IO ()
copyDirectoryFollowingSymlinks src dst = do
  retCode <- system $ "cp -pLR \"" ++ src ++ "\" \"" ++ dst ++ "\""
  if retCode /= 0
    then error $ "Copy failed with " ++ (show retCode)
    else return ()

--- Creates a new symlink.
createSymlink :: String -> String -> IO Int
createSymlink from to = system $ "ln -s " ++ (quote from) ++ " " ++ (quote to)

--- Deletes a symlink.
removeSymlink :: String -> IO Int
removeSymlink link = system $ "rm " ++ quote link

--- Tests whether a file is a symlink.
isSymlink :: String -> IO Bool
isSymlink link = do
  (code, _, _) <- evalCmd "readlink" ["-n", link] ""
  return $ code == 0

--- Gets the target of a symlink.
linkTarget :: String -> IO String
linkTarget link = do
  (rc, out, _) <- evalCmd "readlink" ["-n", link] ""
  if rc == 0
    then return $ replaceFileName link out
    else return ""

--- Puts a file argument into quotes to avoid problems with files containing
--- blanks.
quote :: String -> String
quote s = "\"" ++ s ++ "\""

--- Checks whether a file exists in one of the directories on the PATH.
fileInPath :: String -> IO Bool
fileInPath file = do
  path <- getEnv "PATH"
  let dirs = splitOn ":" path
  (liftIO (any id)) $ mapIO (doesFileExist . (</> file)) dirs

--- Checks whether a file exists in one of the directories on the PATH
--- and returns absolute path, otherwise returns `Nothing`.
getFileInPath :: String -> IO (Maybe String)
getFileInPath file = do
  path <- getEnv "PATH"
  checkPath (splitOn ":" path)
 where
  checkPath [] = return Nothing
  checkPath (dir:dirs) = do
    let dirfile = dir </> file
    ifFileExists dirfile
                 (getAbsolutePath dirfile >>= return . Just)
                 (checkPath dirs)

--- Gets CPM's temporary directory.
tempDir :: IO String
tempDir = do
  t <- getTemporaryDirectory
  return (t </> "cpm")

--- Executes an IO action with the current directory set to  CPM's temporary
--- directory.
inTempDir :: IO b -> IO b
inTempDir b = do
  t <- tempDir
  exists <- doesDirectoryExist t
  if exists
    then return ()
    else createDirectory t
  inDirectory t b

--- Executes an IO action with the current directory set to a specific
--- directory.
inDirectory :: String -> IO b -> IO b
inDirectory dir b = do
  previous <- getCurrentDirectory
  setCurrentDirectory dir
  b' <- b
  setCurrentDirectory previous
  return b'

--- Recreates a directory. Deletes its contents if it already exists.
recreateDirectory :: String -> IO ()
recreateDirectory dir = do
  removeDirectoryComplete dir
  createDirectoryIfMissing True dir

--- Deletes a directory and its contents, if it exists, otherwise nothing
--- is done.
removeDirectoryComplete :: String -> IO ()
removeDirectoryComplete dir = do
  exists <- doesDirectoryExist dir
  when exists $ system ("rm -Rf " ++ quote dir) >> done

--- Reads the complete contents of a file and catches any error
--- (which is returned).
safeReadFile :: String -> IO (Either IOError String)
safeReadFile fname = do
  catch (readCompleteFile fname >>= return . Right)
        (return . Left)

--- Returns the list of all entries in a directory and terminates with
--- an error message if the directory does not exist.
checkAndGetDirectoryContents :: FilePath -> IO [FilePath]
checkAndGetDirectoryContents dir = do
  exdir <- doesDirectoryExist dir
  if exdir then getDirectoryContents dir
           else do putStrLn $ "ERROR: Directory '" ++ dir ++ "' does not exist!"
                   exitWith 1

--- Returns the list of all visible entries in a directory (i.e., not starting
--- with '.') and terminates with an error message if the directory
--- does not exist.
checkAndGetVisibleDirectoryContents :: FilePath -> IO [FilePath]
checkAndGetVisibleDirectoryContents dir =
  checkAndGetDirectoryContents dir >>= return . filter (not . isPrefixOf ".")

--- Performs an action when a file exists.
whenFileExists :: FilePath -> IO () -> IO ()
whenFileExists fname act = do
  exfile <- doesFileExist fname
  when exfile act

--- Performs one of two actions depending on the existence of a file.
ifFileExists :: FilePath -> IO a -> IO a -> IO a
ifFileExists fname thenact elseact = do
  exfile <- doesFileExist fname
  if exfile then thenact else elseact
