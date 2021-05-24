# Keyman Web
The Original Code is (C) 2017-2018 SIL International

Do not merge. Trivial change for triggering PR build...

## Minimum Web Compilation Requirements

* Java 7+ (required by the Google Closure Compiler)
    * This can be ignored for Keyman Developer builds.
* A local installation of [Node.js](https://nodejs.org/) v8.9+.
	* Builds will call `npm install` to automatically install further necessary build dependencies.

	* Linux users can run the following to update to LTS version of nodejs

```
sudo apt-get install python-software-properties
curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash -
sudo apt-get install nodejs
```

**********************************************************************

The following folders contain the distribution for KeymanWeb:

	source                      Source code
	sources/resources/osk       OSK resources for inclusion in mobile app builds;
	                            keymanweb-osk.ttf is maintained at https://github.com/silnrsi/font-keymanweb-osk
	unit_tests                  Automated testing resources

	intermediate    	Intermediate compiled code; TypeScript source is compiled here before it is minified.
	release/web			Fully-compiled KeymanWeb modules for testing
	release/embedded	Fully-compiled KMEA/KMEI modules for inclusion in mobile app builds
	release/unminified/web       Fully-compiled but non-minified KeymanWeb modules
	release/unminified/embedded  Fully-compiled KMEA/KMEI modules for inclusion in mobile app builds

	samples				Sample pages demonstrating ways to link with KeymanWeb
	testing     		Test-case web-pages for various aspects of KeymanWeb functionality
	unit_tests			A Node-driven test suite for automated testing of KeymanWeb

## Usage
Open **index.html** or **samples/index.html** in your browser. Be sure to compile KeymanWeb before viewing the pages.

Refer to the samples for usage details.

To view pages using compiled KeymanWeb,
1. cd to **keyman/web/source**
2. Run `./build.sh`

### `build.sh` options:

*  `-ui`
    - to compile desktop user interface modules to output folder
*  `-test`
    - to compile for testing without copying resources or updating the saved version number.
*  `-embed`
    - to compile only the KMEA/KMEI embedded engine.
*  `-web`
    - to compile only the KeymanWeb engine.
*  `-debug_embedded`
    - to compile a readable version of the embedded KMEA/KMEI code
*  `-no_minify`
    - to disable the minified '/release/web' and '/release/embedded' build sections.
	- the '/release/unminified/web' and '/release/unminified/embedded' folders will still be built.
*  `-clean`
    - to erase pre-existing build products before the build.

If more than one target is specified, the last one will take precedence.

### Unit Testing ###

Before running unit tests on KeymanWeb, first run `./build.sh` according to the instructions above.

Once the build is complete, running `npm test` will run the unit testing suite on your local machine in-browser.  Alternatively, see `unit_tests/test.sh`, which the former command executes.
