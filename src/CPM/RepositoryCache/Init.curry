------------------------------------------------------------------------------
--- Operations to initialize the repository cache database.
---
--- @author Michael Hanus
--- @version March 2018
------------------------------------------------------------------------------

module CPM.RepositoryCache.Init
  ( repositoryCacheDB, tryWriteRepositoryDB )
 where

import Directory    ( removeFile )
import FilePath     ( (</>) )
import ReadShowTerm

import Database.CDBI.ER 
import Database.CDBI.Connection

import CPM.Config      ( Config, readConfigurationWith, repositoryDir )
import CPM.ErrorLogger
import CPM.FileUtil    ( fileInPath, whenFileExists )
import CPM.RepositoryCache.RepositoryDB
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
  if withsqlite then writeRepositoryDB cfg
                else log Debug "Command 'sqlite3' not found"

--- Writes the repository database with the current repository index.
writeRepositoryDB :: Config -> IO (ErrorLogger ())
writeRepositoryDB cfg = do
  let sqlitefile = repositoryCacheDB cfg
  whenFileExists sqlitefile (removeFile sqlitefile)
  createNewDB sqlitefile
  repo <- readRepositoryFrom (repositoryDir cfg)
  add2repodb sqlitefile (allPackages repo)

-- Add a list of package specifications to the database.
add2repodb :: String -> [Package] -> IO (ErrorLogger ())
add2repodb sqlitefile pkgs =
  log Debug ("Writing repository cache DB '" ++ sqlitefile ++ "'") |>
  putStr "Writing repository cache" >>
  mapEL (runDBAction . newEntry) pkgs |> putChar '\n' >>
  log Info "Repository cache DB written"
 where
  runDBAction act = do
    result <- runWithDB sqlitefile act
    case result of
      Left (DBError kind str) -> log Critical $ "Repository DB failure: " ++
                                                show kind ++ " " ++ str
      Right _ -> putChar '.' >> succeedIO ()
  
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
