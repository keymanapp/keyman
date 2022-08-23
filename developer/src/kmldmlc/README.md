Keyman Developer - LDML Keyboard Compiler
================

This package provides the following Keyman **command line tool**:

 - `kmldmlc` — takes **LDML Keyboard .xml sources** and compiles them in to a
   KMXPlus **.kmx** file.

Install
-------

Install `kmlmdlc` globally:

    npm install -g @keymanapp/ldml-keyboard-compiler

Usage
-----

To compile a lexical model from its `.xml` source, use `kmldmlc`:

    kmldmlc my-keyboard.xml --outFile my-keyboard.kmx

To see more command line options by using the `--help` option:

    kmldmlc --help

How to build from source
------------------------

Run `build.sh`:

    ./build.sh configure build


How to run the tests
--------------------

    ./build.sh test


How to prepare a bundle
-----------------------

    ./build.sh bundle
