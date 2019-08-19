Keyman Developer
================

This contains the source code of Keyman development tools. This also
contains `kmlmc`.

In order to build [lexical models][], these tools must be built and
compiled.

[lexical models]: https://github.com/keymanapp/lexical-models


How to build
------------

    ./build.sh


How to run the tests
--------------------

    ./build.sh -test


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
