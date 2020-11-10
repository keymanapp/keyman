Keyman Developer
================

This package provides the following Keyman **command line tools**:

 - `kmlmc` — takes **lexical model sources** and compiles them in to
   a **.js** file.
 - `kmlmp` — uses a `.model.kmp` file to generate a redistributable
   **lexical model package**.
 - `kmlmi` — merges Keyman lexical model `.model_info` files.

`kmlmc` is intended to be used standalone, or as part of a build system.
`kmlmp` is used only by command line tools. `kmlmi` is used exclusively
in the [lexical-models repository][lexical models].

In order to build [lexical models][], these tools must be built and
compiled.

[lexical models]: https://github.com/keymanapp/lexical-models


Install
-------

Install `kmlmc`, `kmlmp`, and `kmlmi` globally:

    npm install -g @keymanapp/lexical-model-compiler

Usage
-----

To compile a lexical model from its `.model.ts` source, use `kmlmc`:

    kmlmc my-lexical-model.model.ts --outFile my-lexical-model.js

To see more command line options by using the `--help` option:

    kmlmc --help
    kmlmp --help
    kmlmi --help

How to build from source
------------------------

Run `build.sh`:

    ./build.sh


How to run the tests
--------------------

    ./build.sh -test

