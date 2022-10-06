# The Curry Package Manager

This repository contains the Curry package manager (CPM). 

## Quick Start

To build the Curry package manager, you need to run `make` inside this 
directory. The `Makefile` assumes that the `curry` executable and `git` are on
your path. If the build was successful, a `cypm` binary will be placed in the 
directory `~/.cpm/bin` (which is also the directory where CPM installs
binaries of tools distributed in packages). Therefore, you should
add this directory to your path. Afterwards, run 
`cypm update` to clone a copy of the central package index repository. More 
information can be found in the manual, see the `docs` directory.

## Documentation

Use `make manual` to generate a PDF version of the manual. A working LaTeX 
installation is required. `make doc` generates the CurryDoc documentation for
the CPM source code in the `cdoc` directory.

## Contributing

Please run the tests using `make test` before publishing your changes. You 
should also run the performance tests when you make changes to the API or
behavior comparison modules or the resolution algorithm. To run the performance
tests, build the performance test program using `make buildperf`. You can then
use `bin/perftest` to execute the different performance test. 

To test the API comparison algorithm, use `bin/perftest api -n NUMBER`, where 
`NUMBER` is the number of added, changed and removed functions and types each 
that you want to compare. Note that when you specify 1000, the API comparison
will compare 6000 elements: 1000 added functions, 1000 removed functions, 1000
changed functions, 1000 added types, 1000 removed types, and 1000 changed types.

The behavior comparison algorithm can be tested using `bin/perftest behavior -t 
T -f F`, where `F` is the number of functions to compare and `T` is the depth to
which the type of each function's argument is nested. For example, if `T` is set 
to 2, each generated function will take a type `Nested1`, which is defined as 
follows:

```haskell
data Nested1 = Nested1 Nested2
data Nested2 = Nested2 String
```

To test the resolution algorithm, you need a set of test data, which you can
find in the [cpm-perf-test-data](1) repository. Make sure that `packages.term`
from thate repository is available in the current directory and then run 
`bin/perftest resolution --packages=P`, where `P` is a comma-separated list of
package identifiers. A complete list of package identifiers available in the
test data set can be found in the `packages.txt` file alongside `packages.term`.
A good set of packages to start with is the following:

- `express-4.14.0` has 1,759 dependencies available in 23,295 different versions.
  Resolution succeeds relatively quickly, since a solution can be found early in
  the candidate tree.
- `express-3.9.0` has 1,794 dependencies available in 23,286 different versions.
  Resolution fails in about a quarter second on KiCS2 since a package is missing
  in the sample data set.
- `chalk-1.1.3` only has 8 dependencies available in 65 different versions. 
  Resolution succeeds very quickly.
- `request-2.74.0` has 1,789 dependencies available in 23,229 different 
  versions. Resolution still succeeds quickly on KiCS2, but takes over a second
  on PAKCS.
- `mocha-1.21.5` has 1,789 dependencies available in 23,229 different versions.
  Resolution fails with a dependency conflict in about 4.5 seconds on KiCS2, but
  fails to finish in a reasonable timeframe on PAKCS.
- `karma-1.2.0` has 1,850 dependencies available in 24,264 different versions.
  Currently, the resolution algorithm is too slow and does not arrive at a 
  solution in a reasonable timeframe.


## Contents of the repository (to be completed)

- `compute-dependencies.txt`: Auxiliary script to compute the contents of
  `dependencies.txt` (see below) if this package is installed by CPM.
  This script is used in case of updates of this package.
- `dependencies.txt`: A textual representation of all Curry packages used
  by CPM. It is used by the script `fetch-dependencies.sh` to build
  an initial version of CPM with `make` (and without another CPM executable).
- `package.json`:
  Since CPM is also implemented in the form of a Curry package,
  this file contains the package specification for CPM.
- `package.schema.json`:
  A description of the format of package specification files used by CPM
  in the format as a [JSON schema](https://json-schema.org/).
  JSON schema is a widely adopted format that makes it easier
  to external tooling such as IDEs to provide validation and
  autocompletion in JSON documents.
