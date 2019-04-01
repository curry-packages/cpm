------------------------------------------------------------------------------
--- Operations to initialize and manipulate the repository cache database.
---
--- @author Michael Hanus
--- @version March 2018
------------------------------------------------------------------------------

module CPM.Repository.CacheDB
  ( repositoryCacheDB, tryWriteRepositoryDB, addPackagesToRepositoryDB )
 where

import System.Directory    ( removeFile )
import System.FilePath     ( (</>) )
import System.IO           ( hFlush, stdout )
import Control.Monad
import Prelude hiding (log)
import ReadShowTerm

import Database.CDBI.ER
import Database.CDBI.Connection
import System.Path  ( fileInPath )

import CPM.Config      ( Config, readConfigurationWith, repositoryDir )
import CPM.ErrorLogger
import CPM.FileUtil    ( whenFileExists )
import CPM.Repository.RepositoryDB
import CPM.Package
import CPM.Repository

--- The database containing the repository cache.
repositoryCacheDB :: Config -> String
repositoryCacheDB cfg = repositoryCacheFilePrefix cfg ++ ".db"

--- Writes the repository database with the current repository index
--- if the command `sqlite3` is in the path.
tryWriteRepositoryDB :: Config -> ErrorLogger ()
tryWriteRepositoryDB cfg = do
  withsqlite <- liftIOErrorLogger $ fileInPath "sqlite3"
  if withsqlite
    then writeRepositoryDB cfg
    else log Info
      "Command 'sqlite3' not found: install package 'sqlite3' to speed up CPM"

--- Writes the repository database with the current repository index.
writeRepositoryDB :: Config -> ErrorLogger ()
writeRepositoryDB cfg = do
  let sqlitefile = repositoryCacheDB cfg
  liftIOErrorLogger $ whenFileExists sqlitefile (removeFile sqlitefile)
  liftIOErrorLogger $ createNewDB sqlitefile
  repo <- readRepositoryFrom (repositoryDir cfg)
  debugMessage $ "Writing repository cache DB '" ++ sqlitefile ++ "'"
  liftIOErrorLogger $ putStr "Writing repository cache DB"
  addPackagesToRepositoryDB cfg False (allPackages repo)
  liftIOErrorLogger $ putChar '\n'
  log Info "Repository cache DB written"

-- Add a list of package specifications to the database.
addPackagesToRepositoryDB :: Config -> Bool -> [Package] -> ErrorLogger ()
addPackagesToRepositoryDB cfg quiet pkgs =
  mapM (runDBAction . newEntry) pkgs >> return ()
 where
  runDBAction act = do
    result <- liftIOErrorLogger $ runWithDB (repositoryCacheDB cfg) act
    case result of
      Left (DBError kind str) -> log Critical $ "Repository DB failure: " ++
                                                show kind ++ " " ++ str
      Right _ -> liftIOErrorLogger
                    (unless quiet $ putChar '.' >> hFlush stdout) >> return ()

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
