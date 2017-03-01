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
  , fileInPath
  , tempDir
  , inTempDir
  , inDirectory
  , recreateDirectory
  , removeDirectoryComplete
  ) where

import Directory ( doesFileExist, getCurrentDirectory, setCurrentDirectory
                 , getTemporaryDirectory, doesDirectoryExist, createDirectory
                 , createDirectoryIfMissing)
import System    (system, getEnviron)
import IOExts    (evalCmd)
import FilePath  (FilePath, replaceFileName, (</>), searchPathSeparator)
import List      (intercalate, splitOn)

--- Joins a list of directories into a search path.
joinSearchPath :: [FilePath] -> String
joinSearchPath dirs = intercalate [searchPathSeparator] dirs

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
removeSymlink link = system $ "rm " ++ (quote link)

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

quote :: String -> String
quote s = "\"" ++ s ++ "\""

--- Checks whether a file exists in one of the directories on the PATH.
fileInPath :: String -> IO Bool
fileInPath file = do
  path <- getEnviron "PATH"
  dirs <- return $ splitOn ":" path
  (liftIO (any id)) $ mapIO (doesFileExist . (</> file)) dirs

--- Gets CPM's temporary directory.
tempDir :: IO String
tempDir = do
  t <- getTemporaryDirectory
  return (t </> "cpm")

--- Executes an IO action with the current directory set to  CPM's temporary 
--- directory.
inTempDir :: IO b -> IO b
inTempDir b = do
  t <- getTemporaryDirectory
  exists <- doesDirectoryExist (t </> "cpm")
  if exists
    then return ()
    else createDirectory (t </> "cpm")
  inDirectory (t </> "cpm") b

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
