# Keyman:  Keyboard Processor Module
The Original Code is (C) 2017-2020 SIL International

The Keyboard Processor module is an internal component of KeymanWeb, seen within this repo at /web/.

## Minimum Compilation Requirements

* A local installation of [Node.js](https://nodejs.org/) v8.9+.
	* Builds will call `npm install` to automatically install further necessary build dependencies.

	* Linux users can run the following to update to LTS version of nodejs
	
```
sudo apt-get install python-software-properties
curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash -
sudo apt-get install nodejs
```

* A globally-installed `lerna` package (`npm install -g lerna`)
  * Builds will call `lerna bootstrap` to setup and manage all needed `npm link`s for within-repo dependencies.

**********************************************************************

### `build.sh` options:

The build script may be found at src/build.sh.

*  `// TODO: enumerate build options`
	

### Unit Testing ###

Before running unit tests on the Keyboard Processor module, first run `./build.sh` according to the instructions above.

Once the build is complete, running `npm test` will run the unit testing suite on your local machine in-browser.  Alternatively, see `unit_tests/test.sh`, which the former command executes.
