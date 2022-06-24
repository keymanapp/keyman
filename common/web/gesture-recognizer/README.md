# Keyman:  Gesture Recognizer Module
The Original Code is (C) 2022-2022 SIL International

The Gesture Recognizer module is an internal component for all of Keyman's Web-based OSKs.  At this time, this is only referenced by
KeymanWeb, seen within this repo at /web/.

## Minimum Compilation Requirements

* A local installation of [Node.js](https://nodejs.org/) v8.9+.
	* Builds will call `npm install` to automatically install further necessary build dependencies.

	* Linux users can run the following to update to LTS version of nodejs
	
```
sudo apt-get install python-software-properties
curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash -
sudo apt-get install nodejs
```

**********************************************************************

The build script may be found at src/build.sh.