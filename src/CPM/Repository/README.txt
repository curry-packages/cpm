This directory contains the implementation of the repository index
cache with term files or a SQLite database.

The file

    IndexDB_ERD.curry

contains the definition of the ER model of the database.

The initial version of the module `RepositoryDB` has been generated with

    > erd2curry --cdbi --db REPOSITORY_CACHE.db IndexDB_ERD.curry

The following changes to the initially generated module were applied
after its generation:

* Rename the module from `RepositoryDB` to `CPM.Repository.RepositoryDB`
  (also in `RepositoryDB_SQLCode.info`).

The actual database queries are defined in the module
`CPM.Repository.Select`.
Since these queries are defined as embedded SQL code which requires
the Curry preprocessor to translate them, the distribution of CPM
contains the already preprocessed module whereas the original
module is stored in file `Select_ORG.curry`. Hence, if one
wants to further develop this part of CPM, one has to follow these steps:

1. Copy the original file:

       > cp Select_ORG.curry Select.curry

2. Make changes to `Select.curry`.

3. Compile CPM

4. Store the changes before committing by:

       > cp Select.curry Select_ORG.curry
       > cp Select.curry.CURRYPP Select.curry
