# KeymanWeb configuration notes

WARNING: these are old configuration notes. See [index.md](../index.md) for current steps.

## Minimum Web Compilation Requirements

* [Java 7+](https://adoptopenjdk.net/releases.html) (required by the Google Closure Compiler)
    * This can be ignored for Keyman Developer builds.
* A local installation of [Node.js](https://nodejs.org/) v8.9+.
	* Builds will call `npm install` to automatically install further necessary build dependencies.

	* Linux users can update to LTS version of nodejs by following instructions on the [NodeSource Distributions](https://github.com/nodesource/distributions#table-of-contents) page.
