Requirements for Curry Compilers used by CPM
============================================

CPM can be used with various Curry compilers.
This documents contains a description of the requirements
to the Curry compiler so that it can be directly used with CPM.

Installing packages
-------------------

When CPM installs packages, it checks for compatibility with
the Curry compiler used (which can be specified as the value
of `CURRY_BIN` in the `.cpmrc` file).
For this purpose, the Curry compiler is queried for its version
by some standard options which must be supported by the compiler.
Assume that `cc` is the executable of the compiler.
Then it must support the following options:

* `cc --compiler-name`: Show the name of the compiler and quit.
* `cc --numeric-version`: Show the compiler version quit.
* `cc --base-version`: Show the version of the base libraries implemented
  by the compiler and quit.

These options can also be combined. In this case the information
is shown in subsequent lines.

    > pakcs --compiler-name --numeric-version --base-version
    pakcs
    3.8.0
    3.3.0

    > kics2 --compiler-name --numeric-version --base-version
    kics2
    3.3.0
    3.3.0


The intermediate files produced by the compiler should be
stored in the directory

    .curry/<compiler-name>-<numeric-version>

relative to the directory of the source file.
If the source file is a hierarchical module,
the same hierarchy is used relative to `.curry`.


Running programs
----------------

When CPM starts a Curry system (via `cypm curry`), it sets
the environment variable `CURRYPATH` to the load path of all
included packages and passes the option `--nocypm` to the
executable of the Curry system. Usually, when a Curry system
is started, it should query CPM (by `cypm deps -p`) to get
the value of `CURRYPATH` to load modules. The option `--nocypm`
is intended to turn off this behavior.


Installing executables
----------------------

If CPM installs an executable, it passes the following options
(REPL commands) to the compiler:

   > cc :set v0 :load MAINMOD :save :quit

(where `v0` is replaced by `v1` in debug mode).
