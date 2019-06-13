------------------------------------------------------------------------------
--- Operations to initialize and manipulate the repository cache database.
---
--- @author Michael Hanus
--- @version June 2019
------------------------------------------------------------------------------

module CPM.Repository.CacheDB
  ( repositoryCacheDB, tryWriteRepositoryDB, addPackagesToRepositoryDB )
 where

import Directory    ( doesFileExist, removeFile )
import FilePath     ( (</>) )
import IO           ( hFlush, stdout )
import ReadShowTerm

import Database.CDBI.ER 
import Database.CDBI.Connection
import System.Path  ( fileInPath )
import Text.CSV

import CPM.Config      ( Config, packageTarFilesURL, readConfigurationWith
                       , repositoryDir )
import CPM.ErrorLogger
import CPM.FileUtil    ( inTempDir, quote, tempDir, whenFileExists )
import CPM.Repository.RepositoryDB
import CPM.Package
import CPM.Repository

--- The database containing the repository cache.
repositoryCacheDB :: Config -> String
repositoryCacheDB cfg = repositoryCacheFilePrefix cfg ++ ".db"

--- The database containing the repository cache.
repositoryCacheCSV :: Config -> String
repositoryCacheCSV cfg = repositoryCacheFilePrefix cfg ++ ".csv"

--- Writes the repository database with the current repository index
--- if the command `sqlite3` is in the path.
tryWriteRepositoryDB :: Config -> Bool -> IO (ErrorLogger ())
tryWriteRepositoryDB cfg writecsv = do
  withsqlite <- fileInPath "sqlite3"
  if withsqlite
    then writeRepositoryDB cfg writecsv
    else log Info
      "Command 'sqlite3' not found: install package 'sqlite3' to speed up CPM"

--- Writes the repository database with the current repository index.
--- If the second argument is `True`, also a CSV file containing the
--- database entries is written.
writeRepositoryDB :: Config -> Bool -> IO (ErrorLogger ())
writeRepositoryDB cfg writecsv = do
  let sqlitefile = repositoryCacheDB cfg
  whenFileExists sqlitefile (removeFile sqlitefile)
  createNewDB sqlitefile
  tmpdir <- tempDir
  let csvfile = tmpdir </> "cachedb.csv"
      csvurl  = packageTarFilesURL cfg ++ "/REPOSITORY_CACHE.csv"
  showExecCmd $ "/bin/rm -f " ++ csvfile
  c <- inTempDir $ showExecCmd $
         "curl -f -s -o " ++ csvfile ++ " " ++ quote csvurl
  csvexists <- doesFileExist csvfile
  pkgentries <- if c == 0 && csvexists
                  then do
                    debugMessage $ "Reading CSV file '" ++ csvfile ++ "'..."
                    readCSVFile csvfile >>= return . map Right
                  else do
                    debugMessage $ "Fetching repository cache CSV file failed"
                    repo <- readRepositoryFrom (repositoryDir cfg)
                    return (map Left (allPackages repo))
  putStr "Writing repository cache DB"
  addPackagesToRepositoryDB cfg False pkgentries
  putChar '\n'
  log Info "Repository cache DB written"
  showExecCmd $ "/bin/rm -f " ++ csvfile
  if writecsv then saveDBAsCSV cfg
              else succeedIO ()

--- Add a list of package descriptions to the database.
--- Here, a package description is either a (reduced) package specification
--- or a list of string (a row from a CSV file) containing the required infos.
addPackagesToRepositoryDB :: Config -> Bool
                          -> [Either Package [String]] -> IO (ErrorLogger ())
addPackagesToRepositoryDB cfg quiet pkgs =
  mapEL (runDBAction . newEntry) pkgs |> succeedIO ()
 where
  runDBAction act = do
    result <- runWithDB (repositoryCacheDB cfg) act
    case result of
      Left (DBError kind str) -> log Critical $ "Repository DB failure: " ++
                                                show kind ++ " " ++ str
      Right _ -> (unless quiet $ putChar '.' >> hFlush stdout) >> succeedIO ()
  
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
saveDBAsCSV :: Config -> IO (ErrorLogger ())
saveDBAsCSV cfg = do
  result <- runWithDB (repositoryCacheDB cfg)
                      (getAllEntries indexEntry_CDBI_Description)
  case result of
    Left (DBError kind str) -> log Critical $ "Repository DB failure: " ++
                                              show kind ++ " " ++ str
    Right es -> do let csvfile = repositoryCacheCSV cfg
                   writeCSVFile csvfile (map showIndexEntry es)
                   log Info ("CSV file '" ++ csvfile ++ "' written!")
 where
  showIndexEntry (IndexEntry _ pn pv deps cc syn cat dirs mods exe) =
    [pn,pv,deps,cc,syn,cat,dirs,mods,exe]
