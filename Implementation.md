Some details about CPM's implementation
========================================

Information about CPM's local storage structure (i.e., on the client side)
can be found in the manual.

Here is some information about CPM's global storage structure.


Global package index
--------------------

CPM requires a global index containing the specifications
of all available packages. The default URLs are defined in
`CPM.Config.packageIndexDefaultURLs`, currently as

    https://cpm.curry-lang.org/PACKAGES/INDEX.tar.gz

This configuration can be changed by the `.cpmrc` value

    PACKAGE_INDEX_URL

It is a gzipped tar file which contains, for each package `pkg`
and version `vers`, a file

    pkg/vers/package.json

This file contains the package specification in JSON format.
For instance, it contains the files

    cpm/2.1.1/package.json
    cpm/3.1.0/package.json
    cpm/3.3.0/package.json

The global package index is downloaded and installed by the CPM command

    > cypm update

This command also downloads a sqlite3 database containing
the most important information about each package.
The database is used by various CPM commands to accelerate
the access to information about packages.


Global package store
--------------------

CPM uses a global store containing a gzipped tar file for each package.
The default URLs are defined in `CPM.Config.packageTarFilesDefaultURLs`,
currently as

    https://cpm.curry-lang.org/PACKAGES

This configuration can be changed by the `.cpmrc` value

    PACKAGE_TARFILES_URL

In order to download the package `pkg` in version `vers`,
CPM extends this URL by the string `pkg-vers.tar.gz`.
For instance, CPM downloads version 3.3.0 of package `cpm` from

    https://cpm.curry-lang.org/PACKAGES/cpm-3.3.0.tar.gz

If CPM cannot download anything from this location,
it tries to download the package from the `source` field
of the package description.


Global package index cache
--------------------------

As mentioned above, CPM keeps the most important information
of all packages in a sqlite3 database which is also downloaded
by the `update` command. The plain contents of the database
in CSV format is also kept in the file

    https://cpm.curry-lang.org/PACKAGES/REPOSITORY_CACHE.csv

This file and the database is generated when packages are published
by [Masala](https://cpm.curry-lang.org/masala/).


Uploading packages
------------------

Currently, new package or package version can be uploaded to these
global stores by the command

    > cypm upload

(see the manual for more details). To upload a package, it should have
a source specification of the form

    "source": {
      "git": "...",
      "tag": "$version"

If one has write access to the source repository, one could also add
the option `--tag` to the `cypm upload` command so that the repository
is tagged with the current version of the package.
This command tests the package and, in case of a successful test,
uploads the package via [Masala](https://cpm.curry-lang.org/masala/)
to the global package index, i.e., one has also to provide the
login and password of the Masala account during the upload process.
