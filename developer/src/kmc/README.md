Keyman Developer - Next Generation Compiler
================

This package provides the following Keyman **command line tools**:

 - `kmc` — takes **LDML Keyboard .xml sources** and compiles them in to a
   KMXPlus **.kmx** file.
 - `kmlmc` — takes **lexical model sources** and compiles them in to a **.js**
   file.
 - `kmlmp` — uses a `.model.kmp` file to generate a redistributable **lexical
   model package**.

`kmlmc` is intended to be used standalone, or as part of a build system. `kmlmp`
is used only by command line tools.

Note: `kmc` will in the future replace `kmlmc` and `kmlmp`.

In order to build [lexical models][], these tools must be built and compiled.

[lexical models]: https://github.com/keymanapp/lexical-models


Install
-------

Install `kmc` globally:

    npm install -g @keymanapp/kmc

kmc Usage
---------

To compile an LDML keyboard from its `.xml` source, use `kmc`:

    kmc build my-keyboard.xml --outFile my-keyboard.kmx

To see more command line options by using the `--help` option:

    kmc --help

---

To compile a lexical model from its `.model.ts` source, use `kmc`:

    kmc build my-lexical-model.model.ts --outFile my-lexical-model.model.js

To see more command line options by using the `--help` option:

    kmc --help

---

kmc can now build package installers for Windows. Example usage (Bash on
Windows, using 'node .' instead of 'kmc' to run the local build):

```
node . build windows-package-installer \
  $KEYMAN_ROOT/developer/src/kmc-package/test/fixtures/khmer_angkor/source/khmer_angkor.kps \
  --msi /c/Program\ Files\ \(x86\)/Common\ Files/Keyman/Cached\ Installer\ Files/keymandesktop.msi \
  --exe $KEYMAN_ROOT/windows/bin/desktop/setup-redist.exe \
  --license $KEYMAN_ROOT/LICENSE.md \
  --out-file ./khmer.exe
```

How to build from source
------------------------

Run `build.sh`:

    ./build.sh configure build

or (less preferably -- build.sh is more efficient):

    nmake configure build

Once you have `configure`d once, you should not normally need to do it again
unless dependencies change or you clean the build folder. `./build.sh` without
parameters will do the default action, which is `build`.

TODO: Note that kmc currently depends on kmc-* to have been configured; while
the build of kmc will do the typescript component of the build, it will not be
able to do any other build steps, so you may wish to build each of the
components separately, one time.

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