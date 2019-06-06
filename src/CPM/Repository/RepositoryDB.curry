--- This file has been generated from
--- 
---     cpm/src/CPM/Repository/IndexDB_ERD.curry
--- 
--- and contains definitions for all entities and relations
--- specified in this model.

module CPM.Repository.RepositoryDB where

import qualified Time
import qualified Database.CDBI.ER
import qualified Database.CDBI.Criteria
import qualified Database.CDBI.Connection
import qualified Database.CDBI.Description

data IndexEntry = IndexEntry IndexEntryID String String String String String String String String String
 deriving (Eq,Show,Read)

data IndexEntryID = IndexEntryID Int
 deriving (Eq,Show,Read)

--- The name of the SQLite database file.
sqliteDBFile :: String
sqliteDBFile = "REPOSITORY_CACHE.db" -- not used

--- The ER description of the `IndexEntry` entity.
indexEntry_CDBI_Description
  :: Database.CDBI.Description.EntityDescription IndexEntry
indexEntry_CDBI_Description =
  Database.CDBI.Description.ED "IndexEntry"
   [Database.CDBI.Connection.SQLTypeInt
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString
   ,Database.CDBI.Connection.SQLTypeString]
   (\(IndexEntry
       (IndexEntryID key)
       name
       version
       dependencies
       compilerCompatibility
       synopsis
       category
       sourceDirs
       exportedModules
       executableSpec) ->
     [Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLString version
     ,Database.CDBI.Description.sqlString dependencies
     ,Database.CDBI.Description.sqlString compilerCompatibility
     ,Database.CDBI.Description.sqlString synopsis
     ,Database.CDBI.Description.sqlString category
     ,Database.CDBI.Description.sqlString sourceDirs
     ,Database.CDBI.Description.sqlString exportedModules
     ,Database.CDBI.Description.sqlString executableSpec])
   (\(IndexEntry
       _
       name
       version
       dependencies
       compilerCompatibility
       synopsis
       category
       sourceDirs
       exportedModules
       executableSpec) ->
     [Database.CDBI.Connection.SQLNull
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLString version
     ,Database.CDBI.Description.sqlString dependencies
     ,Database.CDBI.Description.sqlString compilerCompatibility
     ,Database.CDBI.Description.sqlString synopsis
     ,Database.CDBI.Description.sqlString category
     ,Database.CDBI.Description.sqlString sourceDirs
     ,Database.CDBI.Description.sqlString exportedModules
     ,Database.CDBI.Description.sqlString executableSpec])
   (\[Database.CDBI.Connection.SQLInt key
     ,Database.CDBI.Connection.SQLString name
     ,Database.CDBI.Connection.SQLString version
     ,dependencies
     ,compilerCompatibility
     ,synopsis
     ,category
     ,sourceDirs
     ,exportedModules
     ,executableSpec] ->
     IndexEntry (IndexEntryID key) name version
      (Database.CDBI.Description.fromStringOrNull dependencies)
      (Database.CDBI.Description.fromStringOrNull compilerCompatibility)
      (Database.CDBI.Description.fromStringOrNull synopsis)
      (Database.CDBI.Description.fromStringOrNull category)
      (Database.CDBI.Description.fromStringOrNull sourceDirs)
      (Database.CDBI.Description.fromStringOrNull exportedModules)
      (Database.CDBI.Description.fromStringOrNull executableSpec))

--- The database table of the `IndexEntry` entity.
indexEntryTable :: Database.CDBI.Description.Table
indexEntryTable = "IndexEntry"

--- The database column `Key` of the `IndexEntry` entity.
indexEntryColumnKey :: Database.CDBI.Description.Column IndexEntryID
indexEntryColumnKey =
  Database.CDBI.Description.Column "\"Key\"" "\"IndexEntry\".\"Key\""

--- The database column `Name` of the `IndexEntry` entity.
indexEntryColumnName :: Database.CDBI.Description.Column String
indexEntryColumnName =
  Database.CDBI.Description.Column "\"Name\"" "\"IndexEntry\".\"Name\""

--- The database column `Version` of the `IndexEntry` entity.
indexEntryColumnVersion :: Database.CDBI.Description.Column String
indexEntryColumnVersion =
  Database.CDBI.Description.Column "\"Version\"" "\"IndexEntry\".\"Version\""

--- The database column `Dependencies` of the `IndexEntry` entity.
indexEntryColumnDependencies :: Database.CDBI.Description.Column String
indexEntryColumnDependencies =
  Database.CDBI.Description.Column "\"Dependencies\""
   "\"IndexEntry\".\"Dependencies\""

--- The database column `CompilerCompatibility` of the `IndexEntry` entity.
indexEntryColumnCompilerCompatibility :: Database.CDBI.Description.Column String
indexEntryColumnCompilerCompatibility =
  Database.CDBI.Description.Column "\"CompilerCompatibility\""
   "\"IndexEntry\".\"CompilerCompatibility\""

--- The database column `Synopsis` of the `IndexEntry` entity.
indexEntryColumnSynopsis :: Database.CDBI.Description.Column String
indexEntryColumnSynopsis =
  Database.CDBI.Description.Column "\"Synopsis\"" "\"IndexEntry\".\"Synopsis\""

--- The database column `Category` of the `IndexEntry` entity.
indexEntryColumnCategory :: Database.CDBI.Description.Column String
indexEntryColumnCategory =
  Database.CDBI.Description.Column "\"Category\"" "\"IndexEntry\".\"Category\""

--- The database column `SourceDirs` of the `IndexEntry` entity.
indexEntryColumnSourceDirs :: Database.CDBI.Description.Column String
indexEntryColumnSourceDirs =
  Database.CDBI.Description.Column "\"SourceDirs\""
   "\"IndexEntry\".\"SourceDirs\""

--- The database column `ExportedModules` of the `IndexEntry` entity.
indexEntryColumnExportedModules :: Database.CDBI.Description.Column String
indexEntryColumnExportedModules =
  Database.CDBI.Description.Column "\"ExportedModules\""
   "\"IndexEntry\".\"ExportedModules\""

--- The database column `ExecutableSpec` of the `IndexEntry` entity.
indexEntryColumnExecutableSpec :: Database.CDBI.Description.Column String
indexEntryColumnExecutableSpec =
  Database.CDBI.Description.Column "\"ExecutableSpec\""
   "\"IndexEntry\".\"ExecutableSpec\""

--- The description of the database column `Key` of the `IndexEntry` entity.
indexEntryKeyColDesc :: Database.CDBI.Description.ColumnDescription IndexEntryID
indexEntryKeyColDesc =
  Database.CDBI.Description.ColDesc "\"IndexEntry\".\"Key\""
   Database.CDBI.Connection.SQLTypeInt
   (\(IndexEntryID key) -> Database.CDBI.Connection.SQLInt key)
   (\(Database.CDBI.Connection.SQLInt key) -> IndexEntryID key)

--- The description of the database column `Name` of the `IndexEntry` entity.
indexEntryNameColDesc :: Database.CDBI.Description.ColumnDescription String
indexEntryNameColDesc =
  Database.CDBI.Description.ColDesc "\"IndexEntry\".\"Name\""
   Database.CDBI.Connection.SQLTypeString
   (\name -> Database.CDBI.Connection.SQLString name)
   (\(Database.CDBI.Connection.SQLString name) -> name)

--- The description of the database column `Version` of the `IndexEntry` entity.
indexEntryVersionColDesc :: Database.CDBI.Description.ColumnDescription String
indexEntryVersionColDesc =
  Database.CDBI.Description.ColDesc "\"IndexEntry\".\"Version\""
   Database.CDBI.Connection.SQLTypeString
   (\version -> Database.CDBI.Connection.SQLString version)
   (\(Database.CDBI.Connection.SQLString version) -> version)

--- The description of the database column `Dependencies` of the `IndexEntry` entity.
indexEntryDependenciesColDesc
  :: Database.CDBI.Description.ColumnDescription String
indexEntryDependenciesColDesc =
  Database.CDBI.Description.ColDesc "\"IndexEntry\".\"Dependencies\""
   Database.CDBI.Connection.SQLTypeString
   (\dependencies -> Database.CDBI.Description.sqlString dependencies)
   (\dependencies -> Database.CDBI.Description.fromStringOrNull dependencies)

--- The description of the database column `CompilerCompatibility` of the `IndexEntry` entity.
indexEntryCompilerCompatibilityColDesc
  :: Database.CDBI.Description.ColumnDescription String
indexEntryCompilerCompatibilityColDesc =
  Database.CDBI.Description.ColDesc "\"IndexEntry\".\"CompilerCompatibility\""
   Database.CDBI.Connection.SQLTypeString
   (\compilerCompatibility ->
     Database.CDBI.Description.sqlString compilerCompatibility)
   (\compilerCompatibility ->
     Database.CDBI.Description.fromStringOrNull compilerCompatibility)

--- The description of the database column `Synopsis` of the `IndexEntry` entity.
indexEntrySynopsisColDesc :: Database.CDBI.Description.ColumnDescription String
indexEntrySynopsisColDesc =
  Database.CDBI.Description.ColDesc "\"IndexEntry\".\"Synopsis\""
   Database.CDBI.Connection.SQLTypeString
   (\synopsis -> Database.CDBI.Description.sqlString synopsis)
   (\synopsis -> Database.CDBI.Description.fromStringOrNull synopsis)

--- The description of the database column `Category` of the `IndexEntry` entity.
indexEntryCategoryColDesc :: Database.CDBI.Description.ColumnDescription String
indexEntryCategoryColDesc =
  Database.CDBI.Description.ColDesc "\"IndexEntry\".\"Category\""
   Database.CDBI.Connection.SQLTypeString
   (\category -> Database.CDBI.Description.sqlString category)
   (\category -> Database.CDBI.Description.fromStringOrNull category)

--- The description of the database column `SourceDirs` of the `IndexEntry` entity.
indexEntrySourceDirsColDesc
  :: Database.CDBI.Description.ColumnDescription String
indexEntrySourceDirsColDesc =
  Database.CDBI.Description.ColDesc "\"IndexEntry\".\"SourceDirs\""
   Database.CDBI.Connection.SQLTypeString
   (\sourceDirs -> Database.CDBI.Description.sqlString sourceDirs)
   (\sourceDirs -> Database.CDBI.Description.fromStringOrNull sourceDirs)

--- The description of the database column `ExportedModules` of the `IndexEntry` entity.
indexEntryExportedModulesColDesc
  :: Database.CDBI.Description.ColumnDescription String
indexEntryExportedModulesColDesc =
  Database.CDBI.Description.ColDesc "\"IndexEntry\".\"ExportedModules\""
   Database.CDBI.Connection.SQLTypeString
   (\exportedModules -> Database.CDBI.Description.sqlString exportedModules)
   (\exportedModules ->
     Database.CDBI.Description.fromStringOrNull exportedModules)

--- The description of the database column `ExecutableSpec` of the `IndexEntry` entity.
indexEntryExecutableSpecColDesc
  :: Database.CDBI.Description.ColumnDescription String
indexEntryExecutableSpecColDesc =
  Database.CDBI.Description.ColDesc "\"IndexEntry\".\"ExecutableSpec\""
   Database.CDBI.Connection.SQLTypeString
   (\executableSpec -> Database.CDBI.Description.sqlString executableSpec)
   (\executableSpec ->
     Database.CDBI.Description.fromStringOrNull executableSpec)

--- Gets the attribute `Key` of the `IndexEntry` entity.
indexEntryKey :: IndexEntry -> IndexEntryID
indexEntryKey (IndexEntry a _ _ _ _ _ _ _ _ _) = a

--- Gets the attribute `Name` of the `IndexEntry` entity.
indexEntryName :: IndexEntry -> String
indexEntryName (IndexEntry _ a _ _ _ _ _ _ _ _) = a

--- Gets the attribute `Version` of the `IndexEntry` entity.
indexEntryVersion :: IndexEntry -> String
indexEntryVersion (IndexEntry _ _ a _ _ _ _ _ _ _) = a

--- Gets the attribute `Dependencies` of the `IndexEntry` entity.
indexEntryDependencies :: IndexEntry -> String
indexEntryDependencies (IndexEntry _ _ _ a _ _ _ _ _ _) = a

--- Gets the attribute `CompilerCompatibility` of the `IndexEntry` entity.
indexEntryCompilerCompatibility :: IndexEntry -> String
indexEntryCompilerCompatibility (IndexEntry _ _ _ _ a _ _ _ _ _) = a

--- Gets the attribute `Synopsis` of the `IndexEntry` entity.
indexEntrySynopsis :: IndexEntry -> String
indexEntrySynopsis (IndexEntry _ _ _ _ _ a _ _ _ _) = a

--- Gets the attribute `Category` of the `IndexEntry` entity.
indexEntryCategory :: IndexEntry -> String
indexEntryCategory (IndexEntry _ _ _ _ _ _ a _ _ _) = a

--- Gets the attribute `SourceDirs` of the `IndexEntry` entity.
indexEntrySourceDirs :: IndexEntry -> String
indexEntrySourceDirs (IndexEntry _ _ _ _ _ _ _ a _ _) = a

--- Gets the attribute `ExportedModules` of the `IndexEntry` entity.
indexEntryExportedModules :: IndexEntry -> String
indexEntryExportedModules (IndexEntry _ _ _ _ _ _ _ _ a _) = a

--- Gets the attribute `ExecutableSpec` of the `IndexEntry` entity.
indexEntryExecutableSpec :: IndexEntry -> String
indexEntryExecutableSpec (IndexEntry _ _ _ _ _ _ _ _ _ a) = a

--- Sets the attribute `Key` of the `IndexEntry` entity.
setIndexEntryKey :: IndexEntry -> IndexEntryID -> IndexEntry
setIndexEntryKey (IndexEntry _ b9 b8 b7 b6 b5 b4 b3 b2 b1) a =
  IndexEntry a b9 b8 b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Name` of the `IndexEntry` entity.
setIndexEntryName :: IndexEntry -> String -> IndexEntry
setIndexEntryName (IndexEntry a2 _ b8 b7 b6 b5 b4 b3 b2 b1) a =
  IndexEntry a2 a b8 b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Version` of the `IndexEntry` entity.
setIndexEntryVersion :: IndexEntry -> String -> IndexEntry
setIndexEntryVersion (IndexEntry a3 a2 _ b7 b6 b5 b4 b3 b2 b1) a =
  IndexEntry a3 a2 a b7 b6 b5 b4 b3 b2 b1

--- Sets the attribute `Dependencies` of the `IndexEntry` entity.
setIndexEntryDependencies :: IndexEntry -> String -> IndexEntry
setIndexEntryDependencies (IndexEntry a4 a3 a2 _ b6 b5 b4 b3 b2 b1) a =
  IndexEntry a4 a3 a2 a b6 b5 b4 b3 b2 b1

--- Sets the attribute `CompilerCompatibility` of the `IndexEntry` entity.
setIndexEntryCompilerCompatibility :: IndexEntry -> String -> IndexEntry
setIndexEntryCompilerCompatibility (IndexEntry a5 a4 a3 a2 _ b5 b4 b3 b2 b1) a =
  IndexEntry a5 a4 a3 a2 a b5 b4 b3 b2 b1

--- Sets the attribute `Synopsis` of the `IndexEntry` entity.
setIndexEntrySynopsis :: IndexEntry -> String -> IndexEntry
setIndexEntrySynopsis (IndexEntry a6 a5 a4 a3 a2 _ b4 b3 b2 b1) a =
  IndexEntry a6 a5 a4 a3 a2 a b4 b3 b2 b1

--- Sets the attribute `Category` of the `IndexEntry` entity.
setIndexEntryCategory :: IndexEntry -> String -> IndexEntry
setIndexEntryCategory (IndexEntry a7 a6 a5 a4 a3 a2 _ b3 b2 b1) a =
  IndexEntry a7 a6 a5 a4 a3 a2 a b3 b2 b1

--- Sets the attribute `SourceDirs` of the `IndexEntry` entity.
setIndexEntrySourceDirs :: IndexEntry -> String -> IndexEntry
setIndexEntrySourceDirs (IndexEntry a8 a7 a6 a5 a4 a3 a2 _ b2 b1) a =
  IndexEntry a8 a7 a6 a5 a4 a3 a2 a b2 b1

--- Sets the attribute `ExportedModules` of the `IndexEntry` entity.
setIndexEntryExportedModules :: IndexEntry -> String -> IndexEntry
setIndexEntryExportedModules (IndexEntry a9 a8 a7 a6 a5 a4 a3 a2 _ b1) a =
  IndexEntry a9 a8 a7 a6 a5 a4 a3 a2 a b1

--- Sets the attribute `ExecutableSpec` of the `IndexEntry` entity.
setIndexEntryExecutableSpec :: IndexEntry -> String -> IndexEntry
setIndexEntryExecutableSpec (IndexEntry a10 a9 a8 a7 a6 a5 a4 a3 a2 _) a =
  IndexEntry a10 a9 a8 a7 a6 a5 a4 a3 a2 a

--- id-to-value function for entity `IndexEntry`.
indexEntryID :: IndexEntryID -> Database.CDBI.Criteria.Value IndexEntryID
indexEntryID (IndexEntryID key) = Database.CDBI.Criteria.idVal key

--- id-to-int function for entity `IndexEntry`.
indexEntryKeyToInt :: IndexEntryID -> Int
indexEntryKeyToInt (IndexEntryID key) = key

--- Shows the key of a `IndexEntry` entity as a string.
--- This is useful if a textual representation of the key is necessary
--- (e.g., as URL parameters in web pages), but it should no be used
--- to store keys in other attributes!
showIndexEntryKey :: IndexEntry -> String
showIndexEntryKey entry =
  Database.CDBI.ER.showDatabaseKey "IndexEntry" indexEntryKeyToInt
   (indexEntryKey entry)

--- Transforms a string into a key of a `IndexEntry` entity.
--- Nothing is returned if the string does not represent a meaningful key.
readIndexEntryKey :: String -> Maybe IndexEntryID
readIndexEntryKey = Database.CDBI.ER.readDatabaseKey "IndexEntry" IndexEntryID

--- Gets all `IndexEntry` entities.
queryAllIndexEntrys :: Database.CDBI.Connection.DBAction [IndexEntry]
queryAllIndexEntrys = Database.CDBI.ER.getAllEntries indexEntry_CDBI_Description

--- Gets all `IndexEntry` entities satisfying a given predicate.
queryCondIndexEntry
  :: (IndexEntry -> Bool) -> Database.CDBI.Connection.DBAction [IndexEntry]
queryCondIndexEntry =
  Database.CDBI.ER.getCondEntries indexEntry_CDBI_Description

--- Gets a `IndexEntry` entry by a given key.
getIndexEntry :: IndexEntryID -> Database.CDBI.Connection.DBAction IndexEntry
getIndexEntry =
  Database.CDBI.ER.getEntryWithKey indexEntry_CDBI_Description
   indexEntryColumnKey
   indexEntryID

--- Inserts a new `IndexEntry` entity.
newIndexEntry
  :: String
  -> String
  -> String
  -> String
  -> String
  -> String
  -> String -> String -> String -> Database.CDBI.Connection.DBAction IndexEntry
newIndexEntry
    name_p
    version_p
    dependencies_p
    compilerCompatibility_p
    synopsis_p
    category_p
    sourceDirs_p
    exportedModules_p
    executableSpec_p =
  Database.CDBI.ER.insertNewEntry indexEntry_CDBI_Description setIndexEntryKey
   IndexEntryID
   (IndexEntry (IndexEntryID 0) name_p version_p dependencies_p
     compilerCompatibility_p
     synopsis_p
     category_p
     sourceDirs_p
     exportedModules_p
     executableSpec_p)

--- Deletes an existing `IndexEntry` entry by its key.
deleteIndexEntry :: IndexEntry -> Database.CDBI.Connection.DBAction ()
deleteIndexEntry =
  Database.CDBI.ER.deleteEntry indexEntry_CDBI_Description indexEntryColumnKey
   (indexEntryID . indexEntryKey)

--- Updates an existing `IndexEntry` entry by its key.
updateIndexEntry :: IndexEntry -> Database.CDBI.Connection.DBAction ()
updateIndexEntry = Database.CDBI.ER.updateEntry indexEntry_CDBI_Description

--- Generates a new database (name provided as the parameter) and
--- creates its schema.
createNewDB :: String -> IO ()
createNewDB dbfile =
  do conn <- Database.CDBI.Connection.connectSQLite dbfile
     Database.CDBI.Connection.writeConnection cstr conn
     Database.CDBI.Connection.disconnect conn
  where
    cstr =
      unlines
       ["create table 'IndexEntry'('Key' integer primary key ,'Name' string not null ,'Version' string not null ,'Dependencies' string ,'CompilerCompatibility' string ,'Synopsis' string ,'Category' string ,'SourceDirs' string ,'ExportedModules' string ,'ExecutableSpec' string);"]

--- Saves complete database as term files into an existing directory
--- provided as a parameter.
saveDBTo :: String -> IO ()
saveDBTo dir =
  do Database.CDBI.ER.saveDBTerms indexEntry_CDBI_Description sqliteDBFile dir

--- Restores complete database from term files which are stored
--- in a directory provided as a parameter.
restoreDBFrom :: String -> IO ()
restoreDBFrom dir =
  do Database.CDBI.ER.restoreDBTerms indexEntry_CDBI_Description sqliteDBFile
      dir

--- Runs a DB action (typically a query).
runQ :: Database.CDBI.Connection.DBAction a -> IO a
runQ = Database.CDBI.ER.runQueryOnDB sqliteDBFile

--- Runs a DB action as a transaction.
runT
  :: Database.CDBI.Connection.DBAction a
  -> IO (Database.CDBI.Connection.SQLResult a)
runT = Database.CDBI.ER.runTransactionOnDB sqliteDBFile

--- Runs a DB action as a transaction. Emits an error in case of failure.
runJustT :: Database.CDBI.Connection.DBAction a -> IO a
runJustT = Database.CDBI.ER.runJustTransactionOnDB sqliteDBFile
