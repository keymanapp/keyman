# Keyman Engine for Web
The Original Code is (C) SIL International

## Prerequisites
See [build configuration](../docs/build/index.md) for details on how to
configure your build environment.

**********************************************************************

The following folders contain the distribution for Keyman Engine for Web:

    source                       Source code
    source/resources/osk         OSK resources for inclusion in mobile app builds;
                                 keymanweb-osk.ttf is maintained at https://github.com/silnrsi/font-keymanweb-osk
    unit_tests                   Automated testing resources

    intermediate                 Intermediate compiled code; TypeScript source is compiled here before it is minified.
    release/web                  Fully-compiled KeymanWeb modules for testing
    release/embedded             Fully-compiled KMEA/KMEI modules for inclusion in mobile app builds
    release/unminified/web       Fully-compiled but non-minified KeymanWeb modules
    release/unminified/embedded  Fully-compiled KMEA/KMEI modules for inclusion in mobile app builds

    samples                      Sample pages demonstrating ways to link with KeymanWeb
    testing                      Test-case web-pages for various aspects of KeymanWeb functionality
    unit_tests                   A Node-driven test suite for automated testing of KeymanWeb

**********************************************************************

## Usage
Open **index.html** or **samples/index.html** in your browser. Be sure to
compile Keyman Engine for Web before viewing the pages.

Refer to the samples for usage details.

To view pages using compiled Keyman Engine for Web,
1. cd to **keyman/web/source**
2. Run `./build.sh`

### `build.sh` options:

*  `-ui`
    - to compile desktop user interface modules to output folder
*  `-test`
    - to compile for testing without copying resources or updating the saved
      version number.
*  `-embed`
    - to compile only the KMEA/KMEI embedded engine.
*  `-web`
    - to compile only the KeymanWeb engine.
*  `-debug_embedded`
    - to compile a readable version of the embedded KMEA/KMEI code
*  `-no_minify`
    - to disable the minified '/release/web' and '/release/embedded' build
      sections.
    - the '/release/unminified/web' and '/release/unminified/embedded' folders
      will still be built.
*  `-clean`
    - to erase pre-existing build products before the build.

If more than one target is specified, the last one will take precedence.

### Unit Testing ###

Before running unit tests on Keyman Engine for Web, first run `./build.sh`
according to the instructions above.

Once the build is complete, running `npm test` will run the unit testing suite
on your local machine in-browser. Alternatively, see `unit_tests/test.sh`, which
the former command executes.

### Debugging Unit Tests
1. During development, to run a specific unit test, change the `it` to
   `it.only`. You can also run all tests under a specific group with
   `describe.only`.
2. From this directory, run `./unit_tests/test.sh -debug`. Alternatively, from
   `web/` or any `web/` subdirectory,
   
   ```
   npm run test -- -debug
   ```

   The `--` part tells `npm` to funnel anything to the script as the script's
   command-line parameters. As long as it's run from somewhere within the `web/`
   folder's hierarchy, that line will always run from `web/`, as that's where
   `package.json` is.

3. When the browser halts, click the "Debug" button which opens a new debugging
   tab.
4. In the Dev console, you can set a breakpoint in your test and refresh the
   page to debug
