# `@keymanapp/resources-gosh`

`gosh` is a simple platform-agnostic launcher for bash scripts. On Windows, it will locate an installation of Git for Windows and use bash from that script. On \*nix, it uses the system-supplied version of bash.

> **TODO**: it would be possible for gosh to grab a newer version of bash on macOS which could simplify some of our cross-platform scripts?

## Usage

`gosh` can be run as an npm module command or from the command line with the `gosh`/`gosh.bat` pair. The first parameter should be the bash script to run, and all subsequent parameters passed to gosh will be passed to the script unmodified. Note that paths passed to gosh should always use forward slashes (`/`), irrespective of the platform.

## Examples

### `package.json`

Invoke bash scripts using `gosh` instead:

```json
  "scripts": {
    "build": "gosh ./build.sh",
    "test": "gosh ./unit_tests/test.sh"
  },
```

* `TeamCity instantiation`: (path = `web/source`) `../../resources/gosh/gosh ./build.sh -publish-sentry`
