------------------------------------------------------------------------------
--- ERD specification for the repository index database
---
--- @author Michael Hanus
--- @version March 2018
------------------------------------------------------------------------------

module CPM.Repository.IndexDB_ERD where

import Database.ERD

picERD :: ERD
picERD =
 ERD "RepositoryDB"
   [Entity "IndexEntry"
     [Attribute "Name"                  (StringDom Nothing) NoKey False,
      Attribute "Version"               (StringDom Nothing) NoKey False,
      Attribute "Dependencies"          (StringDom Nothing) NoKey True,
      Attribute "CompilerCompatibility" (StringDom Nothing) NoKey True,
      Attribute "Synopsis"              (StringDom Nothing) NoKey True,
      Attribute "Category"              (StringDom Nothing) NoKey True,
      Attribute "SourceDirs"            (StringDom Nothing) NoKey True,
      Attribute "ExportedModules"       (StringDom Nothing) NoKey True,
      Attribute "ExecutableSpec"        (StringDom Nothing) NoKey True
     ]]
   []

{-

Generate CDBI API with:

    > erd2curry --cdbi --db REPOSITORY_CACHE.db IndexDB_ERD.curry

Manual changes after generating the API module:

* Rename CDBI API module: RepositoryDB -> CPM.RepositoryCache.RepositoryDB

-}
