------------------------------------------------------------------------------
--- Operations to initialize and manipulate the repository cache database.
---
--- @author Michael Hanus
--- @version June 2019
------------------------------------------------------------------------------

module CPM.Repository.CacheDB
  ( repositoryCacheDB, tryInstallRepositoryDB, addPackagesToRepositoryDB )
 where

import System.Directory    ( doesFileExist, removeFile )
import System.FilePath     ( (</>) )
import System.IO           ( hFlush, stdout )
import Control.Monad
import Prelude hiding (log)
import ReadShowTerm

import Database.CDBI.ER
import Database.CDBI.Connection
import System.Path  ( fileInPath )
import Text.CSV

import CPM.Config      ( Config, packageTarFilesURLs, readConfigurationWith
                       , repositoryDir )
import CPM.ErrorLogger
import CPM.FileUtil    ( cleanTempDir, inTempDir, quote, tempDir
                       , whenFileExists )
import CPM.Repository.RepositoryDB
import CPM.Package
import CPM.Repository

--- The database containing the repository cache.
repositoryCacheDB :: Config -> String
repositoryCacheDB cfg = repositoryCacheFilePrefix cfg ++ ".db"

--- The database containing the repository cache.
repositoryCacheCSV :: Config -> String
repositoryCacheCSV cfg = repositoryCacheFilePrefix cfg ++ ".csv"

--- Installs the repository database with the current repository index
--- if the command `sqlite3` is in the path.
tryInstallRepositoryDB :: Config -> Bool -> Bool -> ErrorLogger ()
tryInstallRepositoryDB cfg usecache writecsv = do
  withsqlite <- liftIOEL $ fileInPath "sqlite3"
  if withsqlite
    then installRepositoryDB cfg usecache writecsv
    else infoMessage
      "Command 'sqlite3' not found: install package 'sqlite3' to speed up CPM"

--- Writes the repository database with the current repository index.
--- First, it is tried to download `REPOSITORY_CACHE.db`
--- from the tar files URL (if the second argument is `True`).
--- Otherwise, `writeRepositoryDB` is called.
--- If the second argument is `True`, also a CSV file containing the
--- database entries is written.
installRepositoryDB :: Config -> Bool -> Bool -> ErrorLogger ()
installRepositoryDB cfg False writecsv = writeRepositoryDB cfg False writecsv
installRepositoryDB cfg True  writecsv = do
  let sqlitefile = repositoryCacheDB cfg
  liftIOEL $ whenFileExists sqlitefile $ removeFile sqlitefile
  c <- tryDownloadFromURLs sqlitefile (packageTarFilesURLs cfg)
                           "REPOSITORY_CACHE.db"
  dbexists <- liftIOEL $ doesFileExist sqlitefile
  if c == 0 && dbexists
    then if writecsv then saveDBAsCSV cfg
                     else return ()
    else writeRepositoryDB cfg True writecsv

--- Tries to download some target file (first argument) from a list of
--- base URLs where the source file (third argument) is located.
--- Returns 0 if the download was successfull.
tryDownloadFromURLs :: String -> [String] -> String -> ErrorLogger Int
tryDownloadFromURLs _      []                 _    = return 1
tryDownloadFromURLs target (baseurl:baseurls) file = do
  let sourceurl = baseurl ++ "/" ++ file
  rc <- inTempDirEL $ showExecCmd $
          "curl -f -s -o " ++ quote target ++ " " ++ quote sourceurl
  if rc == 0
    then return 0
    else tryDownloadFromURLs target baseurls file

--- Writes the repository database with the current repository index.
--- It is generated either from the CSV file `REPOSITORY_CACHE.csv`
--- downloaded from the tar files URL (if the second argument is `True`)
--- or from reading all package specs.
--- If the third argument is `True`, also a CSV file containing the
--- database entries is written.
writeRepositoryDB :: Config -> Bool -> Bool -> ErrorLogger ()
writeRepositoryDB cfg usecache writecsv = do
  let sqlitefile = repositoryCacheDB cfg
  liftIOEL $ do
    whenFileExists sqlitefile (removeFile sqlitefile)
    createNewDB sqlitefile
  tmpdir <- liftIOEL $ tempDir
  let csvfile = tmpdir </> "cachedb.csv"
  showExecCmd $ "/bin/rm -f " ++ csvfile
  c <- if usecache
         then tryDownloadFromURLs csvfile (packageTarFilesURLs cfg)
                                  "REPOSITORY_CACHE.csv"
         else return 1
  csvexists <- liftIOEL $ doesFileExist csvfile
  pkgentries <- if c == 0 && csvexists
                  then do
                    debugMessage $ "Reading CSV file '" ++ csvfile ++ "'..."
                    (liftIOEL $ readCSVFile csvfile) >>= return . map Right
                  else do
                    when usecache $ debugMessage $
                      "Fetching repository cache CSV file failed"
                    repo <- readRepositoryFrom (repositoryDir cfg)
                    return $ map Left $ allPackages repo
  liftIOEL $ putStr "Writing repository cache DB"
  addPackagesToRepositoryDB cfg False pkgentries
  liftIOEL $ putChar '\n'
  infoMessage "Repository cache DB written"
  liftIOEL $ cleanTempDir
  if writecsv then saveDBAsCSV cfg 
              else return ()

--- Add a list of package descriptions to the database.
--- Here, a package description is either a (reduced) package specification
--- or a list of string (a row from a CSV file) containing the required infos.
addPackagesToRepositoryDB :: Config -> Bool
                          -> [Either Package [String]] -> ErrorLogger ()
addPackagesToRepositoryDB cfg quiet pkgs =
  mapM (runDBAction . newEntry) pkgs >> return ()
 where
  runDBAction act = do
    result <- liftIOEL $ runWithDB (repositoryCacheDB cfg) act
    case result of
      Left (DBError kind str) -> criticalMessage $ "Repository DB failure: " ++
                                                   show kind ++ " " ++ str
      Right _ -> liftIOEL $ do
        unless quiet $ putChar '.'
        hFlush stdout
        return ()
  
  newEntry (Left p) = newIndexEntry
    (name p)
    (showTerm (version p))
    (showTerm (dependencies p))
    (showTerm (compilerCompatibility p))
    (synopsis p)
    (showTerm (category p))
    (showTerm (sourceDirs p))
    (showTerm (exportedModules p))
    (showTerm (executableSpec  p))
  newEntry (Right [pn,pv,deps,cc,syn,cat,dirs,mods,exe]) =
    newIndexEntry pn pv deps cc syn cat dirs mods exe


--- Saves complete database as term files into an existing directory
--- provided as a parameter.
saveDBAsCSV :: Config -> ErrorLogger ()
saveDBAsCSV cfg = do
  result <- liftIOEL $ runWithDB (repositoryCacheDB cfg)
                                          (getAllEntries indexEntry_CDBI_Description)
  case result of
    Left (DBError kind str) -> criticalMessage $ "Repository DB failure: " ++
                                                 show kind ++ " " ++ str
    Right es -> do let csvfile = repositoryCacheCSV cfg
                   liftIOEL $ writeCSVFile csvfile $ map showIndexEntry es
                   infoMessage ("CSV file '" ++ csvfile ++ "' written!")
 where
  showIndexEntry (IndexEntry _ pn pv deps cc syn cat dirs mods exe) =
    [pn,pv,deps,cc,syn,cat,dirs,mods,exe]
