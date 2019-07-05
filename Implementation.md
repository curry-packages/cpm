Hints about CPM's Implementation
================================

Information about CPM's local storage structure can be found in the manual.

Here is some information about CPM's global storage structure.


Global package index
--------------------

CPM requires a global index containing the specifications
of all available packages. The default URL is defined in
`CPM.Config.packageIndexDefaultURL`, currently as

    https://git.ps.informatik.uni-kiel.de/curry-packages/cpm-index.git

This configuration can be changed by the `.cpmrc` value

    PACKAGE_INDEX_URL

Currently, it is a git repository but it could also be a tar file
or a gzipped tar file.

The directory referenced by this URL must contains for each
package `pkg` and version `vers` a file

    pkg/vers/package.json

containing the package specification in JSON format.
For instance, it contains the files

    cpm/2.0.0/package.json
    cpm/2.1.0/package.json
    cpm/2.1.1/package.json


Global package store
--------------------

CPM uses a global store containing a gzipped tar file for each package.
The default URL is defined in `CPM.Config.packageTarFilesDefaultURL`,
currently as

    https://www.informatik.uni-kiel.de/~curry/cpm/PACKAGES/

This configuration can be changed by the `.cpmrc` value

    PACKAGE_TARFILES_URL

Currently, it is a git repository but it could also be a tar file
or a gzipped tar file.

In order to download the package `pkg` in version `vers`,
CPM extends this URL by the string `pkg-vers.tar.gz`.
For instance, CPM downloads version 2.1.0 of the package `cpm` from

    https://www.informatik.uni-kiel.de/~curry/cpm/PACKAGES/cpm-2.1.0.tar.gz

If CPM cannot download anything from this location,
it tries to download the package from the `source` field
of the package description.


Uploading packages
------------------

Currently, new package or package version can be uploaded to these
global stores by the command

    cypm upload

(see the manual for more details). Currently, only packages having
a source specification of the form

    "source": {
      "git": "...git.ps.informatik.uni-kiel.de/curry-packages/....git",
      "tag": "$version"

can be uploaded. Furthermore, one has to have write access to the
source repository. This command tests the package and, in case
of a successful test, uploads the package to the global package
index and store via the web script at URL

    https://www-ps.informatik.uni-kiel.de/~mh/cpm-upload.cgi

