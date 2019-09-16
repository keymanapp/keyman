Keyman Developer
================

This contains the source code of Keyman lexical model development tools:

 - `kmlmc`
 - `kmlmp`
 - `kmlmi`

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

In a different repository, do this:

    npm install @keymanapp/developer-lexical-model-compiler

If you're adding this to a subproject within
<https://github.com/keymanapp/keyman>, use [lerna][] from the root
directory:

    # In root directory of the repository
    npx lerna add @keymanapp/developer-lexical-model-compiler path/to/my-subproject


[lerna]: https://github.com/lerna/lerna#readme


How to get the command line tools
---------------------------------

If you do not have access to the command line tools, run this within
this directory:

    npm link


How to use on the command line
------------------------------

With the package installed with the above instructions, you can now run
`kmlmc` (Keyman Lexical Model Compiler):

    kmlmc <your model.ts file> -o <compiled lexical model>
