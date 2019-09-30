Keyman Developer
================

This contains the source code of Keyman development tools:

 - `kmlmc` — takes **lexical model sources** and compiles them in to
   a **.js** file.
 - `kmlmp` — uses a `.model.kmp` file to generate a redistributable
   **lexical model package**.
 - `kmlmi` — merges Keyman lexical model `.model_info` files.

`kmlmc` is intended to be used standalone, or as part of a build system.
`kmlmp` is used only by command line tools. `kmlmi` is used only in the
[lexical-models repository](https://github.com/keymanapp/lexical-models).

In order to build [lexical models][], these tools must be built and
compiled.

[lexical models]: https://github.com/keymanapp/lexical-models


How to build
------------

    ./build.sh


How to run the tests
--------------------

    ./build.sh -test


How to update the package version
---------------------------------

**NOTE**: this step should only be performed on the CI server:

    ./build.sh -version MAJOR.MINOR.${BUILD_NUMBER} [-tier (alpha|beta)]


How to install
--------------

This will install both `kmlmc` and will let you install the compiler in
other node projects on your machine.

    ./build.sh
    npm link


How to use in other Node projects
---------------------------------

With the package installed with the above instructions, in your new
project, link this package:


    npm link @keymanapp/developer-lexical-model-compiler


How to use on the command line
------------------------------

With the package installed with the above instructions, you can now run
`kmlmc` (Keyman Lexical Model Compiler):

    kmlmc <your model.ts file> -o <compiled lexical model>
