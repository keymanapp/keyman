# Keyman Web
The Original Code is (C) 2017 SIL International

## Minimum Web Compilation Requirements

* A local installation of [Node.js](https://nodejs.org/).
	* Builds will call `npm install` to automatically install further necessary build dependencies.
* Java 7+ (required by the Google Closure Compiler)

**********************************************************************

The following folders contain the distribution for KeymanWeb:

	source			Source code
	build			Intermediate compiled code; TypeScript source is compiled here before it is minified.
	output			Fully-compiled KeymanWeb modules for testing
	embedded		Fully-compiled KMEA/KMEI modules for inclusion in mobile app builds
	samples			Sample pages demonstrating ways to link with KeymanWeb
  testing     Test-case web-pages for various aspects of KeymanWeb functionality

## Usage
Open **index.html** or **samples/index.html** in your browser. The pages using uncompiled KeymanWeb should work as-is.

To view pages using compiled KeymanWeb,
1. cd to **keyman/web/source**
2. Run `/.build.sh`

Refer to the samples for usage details.
