# Keyman Web
The Original Code is (C) 2017 SIL International

## Minimum Web Compilation Requirements

* A local installation of [Node.js](https://nodejs.org/).
	* Builds will call `npm install` to automatically install further necessary build dependencies.
* Java 7+ (required by the Google Closure Compiler for minification builds)

**********************************************************************

The following folders contain the distribution for KeymanWeb:

	source				         Source code
	intermediate    	         Intermediate compiled code; TypeScript source is compiled here before it is minified.
	release/web	                 Fully-compiled KeymanWeb modules
	release/embedded	         Fully-compiled KMEA/KMEI modules for inclusion in mobile app builds
	release/unminified/web       Fully-compiled but non-minified KeymanWeb modules
	release/unminified/embedded  Fully-compiled KMEA/KMEI modules for inclusion in mobile app builds
	samples				         Sample pages demonstrating ways to link with KeymanWeb
	testing     		         Test-case web-pages for various aspects of KeymanWeb functionality

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