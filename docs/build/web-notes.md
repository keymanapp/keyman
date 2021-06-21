# KeymanWeb configuration notes

## Minimum Web Compilation Requirements

* [Java 7+](https://adoptopenjdk.net/releases.html) (required by the Google Closure Compiler)
    * This can be ignored for Keyman Developer builds.
* A local installation of [Node.js](https://nodejs.org/) v8.9+.
	* Builds will call `npm install` to automatically install further necessary build dependencies.

	* Linux users can run the following to update to LTS version of nodejs

```
sudo apt-get install python-software-properties
curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash -
sudo apt-get install nodejs
```
