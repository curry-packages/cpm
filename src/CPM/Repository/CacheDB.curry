------------------------------------------------------------------------------
--- Operations to initialize and manipulate the repository cache database.
---
--- @author Michael Hanus
--- @version March 2018
------------------------------------------------------------------------------

module CPM.Repository.CacheDB
  ( repositoryCacheDB, tryWriteRepositoryDB, addPackagesToRepositoryDB )
 where

import Directory    ( removeFile )
import FilePath     ( (</>) )
import IO           ( hFlush, stdout )
import ReadShowTerm

import Database.CDBI.ER 
import Database.CDBI.Connection

import CPM.Config      ( Config, readConfigurationWith, repositoryDir )
import CPM.ErrorLogger
import CPM.FileUtil    ( fileInPath, whenFileExists )
import CPM.Repository.RepositoryDB
import CPM.Package
import CPM.Repository

--- The database containing the repository cache.
repositoryCacheDB :: Config -> String
repositoryCacheDB cfg = repositoryCacheFilePrefix cfg ++ ".db"

--- Writes the repository database with the current repository index
--- if the command `sqlite3` is in the path.
tryWriteRepositoryDB :: Config -> IO (ErrorLogger ())
tryWriteRepositoryDB cfg = do
  withsqlite <- fileInPath "sqlite3"
  if withsqlite
    then writeRepositoryDB cfg
    else log Info
      "Command 'sqlite3' not found: install package 'sqlite3' to speed up CPM"

--- Writes the repository database with the current repository index.
writeRepositoryDB :: Config -> IO (ErrorLogger ())
writeRepositoryDB cfg = do
  let sqlitefile = repositoryCacheDB cfg
  whenFileExists sqlitefile (removeFile sqlitefile)
  createNewDB sqlitefile
  repo <- readRepositoryFrom (repositoryDir cfg)
  debugMessage $ "Writing repository cache DB '" ++ sqlitefile ++ "'"
  putStr "Writing repository cache DB"
  addPackagesToRepositoryDB cfg False (allPackages repo)
  putChar '\n'
  log Info "Repository cache DB written"

-- Add a list of package specifications to the database.
addPackagesToRepositoryDB :: Config -> Bool -> [Package] -> IO (ErrorLogger ())
addPackagesToRepositoryDB cfg quiet pkgs =
  mapEL (runDBAction . newEntry) pkgs |> succeedIO ()
 where
  runDBAction act = do
    result <- runWithDB (repositoryCacheDB cfg) act
    case result of
      Left (DBError kind str) -> log Critical $ "Repository DB failure: " ++
                                                show kind ++ " " ++ str
      Right _ -> (unless quiet $ putChar '.' >> hFlush stdout) >> succeedIO ()
  
  newEntry p = newIndexEntry
    (name p)
    (showTerm (version p))
    (showTerm (dependencies p))
    (showTerm (compilerCompatibility p))
    (synopsis p)
    (showTerm (category p))
    (showTerm (sourceDirs p))
    (showTerm (exportedModules p))
    (showTerm (executableSpec  p))
