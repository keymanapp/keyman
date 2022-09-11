Keyman Developer - LDML Keyboard Compiler
================

This package provides the following Keyman **command line tool**:

 - `kmc` — takes **LDML Keyboard .xml sources** and compiles them in to a
   KMXPlus **.kmx** file.

Install
-------

Install `kmlmdlc` globally:

    npm install -g @keymanapp/ldml-keyboard-compiler

Usage
-----

To compile an LDML keyboard from its `.xml` source, use `kmc`:

    kmc my-keyboard.xml --outFile my-keyboard.kmx

To see more command line options by using the `--help` option:

    kmc --help

How to build from source
------------------------

Run `build.sh`:

    ./build.sh configure build

or

    nmake configure build

Once you have `configure`d once, you should not normally need to do it
again unless dependencies change. `./build.sh` without parameters will
do the default action, which is `build`.

How to run the tests
--------------------

    ./build.sh test


How to prepare bundling for installation
----------------------------------------

    ./build.sh bundle --build-path <temp_path>

The temp_path must be a path outside the repository to avoid npm getting
confused by the root package.json. This is called by inst/download.in.mak
normally when building the Keyman Developer installer.

How to publish to NPM
---------------------

    ./build.sh publish [--dry-run]

Publishes the current release to NPM. This should only be run from CI.