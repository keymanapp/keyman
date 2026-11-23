# Dependencies

This document gives a rough overview of the versions of dependencies used in the
current and in the next version of Keyman. The exact versions should be looked
up in [minimum-versions.md](minimum-versions.md), the `package.json` files and
other places.

The most important information in this document is the list of dependencies that
have cross-branch impact due to installation on CI agents.

This document should be updated when planning the next version.

## What version to choose

- Use the latest released version (or latest version with long-term
  support (LTS)), unless there are reasons for using an older version, e.g.
  - A bug in the latest version that prevents Keyman from working/building
    correctly
  - Breaking changes. Migrating to the latest version would require
    significant work and should be scheduled separately.
- If not using the latest version, document the reasons here in this
  document
- If not using the latest version, use the highest possible version
- If at all possible, choose a version that is still supported and still
  receives bugfixes etc. Ideally it would be supported during the
  entire life cycle of the version, i.e. for version 20 it would be
  supported until we release version 21.

## Opening Remarks

- The **v19** and **v20** columns in the following tables mean the
  version of the component used in Keyman 19 (actual) and Keyman 20 (intention).
- The **cross branch CI impact** column marks components that can
  only be installed once on a build agent. Updating the component
  might require changes to both the stable and the master (alpha) 
  branch, and may also impact builds on feature branches.

## Target operating system and platform versions

Target                                   | v19   | v20
-----------------------------------------|-------|------------
KEYMAN_MIN_TARGET_VERSION_ANDROID        | 5     | 8
KEYMAN_MIN_TARGET_VERSION_IOS            | 15.6  | 15.6
KEYMAN_MIN_TARGET_VERSION_WINDOWS        | 10    | 10
KEYMAN_MIN_TARGET_VERSION_MAC            | 13.0  | 13 (or 14 if reasons)
KEYMAN_MIN_TARGET_VERSION_UBUNTU         | 22.04 | 24.04
KEYMAN_MIN_TARGET_VERSION_ANDROID_CHROME | 95.0  | 138

## Target web browsers for KeymanWeb

Target                                | v19  | v20
--------------------------------------|------|----------------------------
KEYMAN_MIN_TARGET_VERSION_WEB_CHROME  | 95.0 | 138
KEYMAN_MIN_TARGET_VERSION_WEB_FIREFOX | 79.0 | [caniuse](https://caniuse.com/)
KEYMAN_MIN_TARGET_VERSION_WEB_OPERA   | 58.0 | [caniuse](https://caniuse.com/)
KEYMAN_MIN_TARGET_VERSION_WEB_SAFARI  | 13.0 | 15.6

## Dependency minimum versions

Target                               | v19    | v20                 | cross branch CI impact|
-------------------------------------|--------|---------------------|:---------------------:|
KEYMAN_MIN_VERSION_NODE_MAJOR        | 20     | 26                  |                       |
KEYMAN_MIN_VERSION_NPM               | 10.5.1 | 12.2.1              |                       |
KEYMAN_MIN_VERSION_EMSCRIPTEN        | 3.1.64 | 6.0.13+ if possible |                       |
KEYMAN_MIN_VERSION_VISUAL_STUDIO     | 2022   | 2026                |     X                 |
KEYMAN_MIN_VERSION_MESON             | 1.0.0  | 1.3.2               |     X [1]             |
KEYMAN_VERSION_XCODE                 | 26.2   | no change           |     X                 |
KEYMAN_VERSION_GRADLE                | 8.12   | 9.8.2+ if possible  |     ?                 |
Android Studio                       |        | latest              |     ?                 |
Delphi                               |        | no change           |     X                 |
Sentry CLI                           | 2.31.0 | 3.8.2 (@sentry/cli) |    [2]                |
Sentry - Android                     | 5.9.0  | 8.62.0              |                       |
Sentry - iOS                         | 8.58   | 9.31.0              |                       |
Sentry - Python (Linux)              | >= 1.1 | >=1.39.2            |                       |
Sentry - macOS (Swift / ObjC)        | 8.57.3 | 9.31.0              |                       |
Sentry - web/js (@sentry/browser)    | 8.55.0 | 11.9.0              |                       |
Sentry - Developer/js (@sentry/node) | 7.57.0 | 11.9.0              |                       |
Sentry - PHP                         | 3.22.1 | 4.34.0              |                       |
Sentry - native Windows (+Delphi)    | 0.6.0  | 0.17.4              |                       |
Crowdin CLI                          | v4     | no change           |     X                 |
GHA runner version - Linux           |        |                     |                       |
GHA runner version - Windows         |        |                     |                       |
WiX (Windows Installer XML)          |        |                     |     X                 |
Carthage (macOS, iOS)                | | replace with Swift Package Manager |   X           |
Cocoapods (macOS)                    | | replace with Swift Package Manager |   X           |
Python 2.7 macOS                     |        | remove              |     X                 |
esbuild                              | 0.28.1 | 0.28.1 or .2        |                       |

- [1] Meson 1.3.2 is the max version available on Ubuntu 24.04
- [2] remove Sentry CLI 1.70 from build agents. We stick with the older
      `@sentry/cli` component for v20 instead of the new `sentry` CLI component.

## Deployed node modules

### Used by _kmc | common_

Target          | v19                              | v20
----------------|----------------------------------|---------------
commander       | 10.0.0 / 5.1.0 / 2.20.0 / 2.19.0 | 15.0.0
fast-xml-parser | 5.8.0                            | 5.11.2
restructure     | 3.0.1                            | 3.0.2
eventemitter3   | 5.0.0                            | 5.0.4
path-browserify | 1.0.1                            | 1.0.1
sax             | 0.6.0                            | 1.6.1
semver          | 7.5.4                            | 7.8.5
xmlbuilder      | 11.0.0                           | 15.1.1
chalk           | 2.4.2                            | 6.0.1
jszip           | 3.7.0 / 3.10.1                   | 3.10.2
marked          | 7.0.0 / 14.1.2                   | 18.1.2

### Used by _developer/server_

Target          | v19                              | v20
----------------|----------------------------------|---------------
express         | 5.2.1 / 4.16.4                   | 5.3.0
multer          | 2.4.0                            | 2.5.0
open            | 8.4.0                            | 11.0.4
ws              | 8.21.0                           | 8.22.1
@ngrok/ngrok    | 1.7.0                            | 1.7.0

### Used by _developer/tike_

Target          | v19                              | v20
----------------|----------------------------------|---------------
monaco-editor   | 0.15.6                           | no change

### Used by _web_

Target                       | v19    | v20
-----------------------------|--------|--------------------
core-js                      | 3.34.0 | remove if possible
tslib                        | 2.5.2  | remove if possible
string.prototype.codepointat | 0.2.1  | remove if possible
string.prototype.startswith  | 0.2.0  | remove if possible

## Dependencies with fixed versions

Target                 | v19  | v20
-----------------------|------|------
KEYMAN_VERSION_ICU     | 73.1 | 79.1
KEYMAN_MIN_VERSION_ICU |      | 74.2

Ubuntu 24.04 ships with 74.2, Ubuntu 26.04 with 78.2. Don't use ICU
features that require ICU > 74.2.

## Language and runtime versions

Target                                 | v19   | v20      | cross branch CI impact|
---------------------------------------|-------|----------|:---------------------:|
KEYMAN_VERSION_JAVA                    | 21    | 25       |         X             |
KEYMAN_MIN_VERSION_CPP                 | 17    | 17       |         X             |
KEYMAN_MIN_VERSION_ANDROID_SDK         | 21    | 26       |                       |
Swift                                  |       | 5 [1]    |                       |
Typescript                             | 5.4.x | 7.0.2 [2]|                       |
Target and lib in tsconfig.json files  | es6   | es2022   |                       |

- [1] Use Swift 5 unless reasons for higher version
- [2] Or Typescript 6 if running into problems with 7

## Default version used in Docker containers

Target                                  | v19   | v20
----------------------------------------|-------|-------
KEYMAN_DEFAULT_VERSION_UBUNTU_CONTAINER | 24.04 | 26.04

## History

- 2026-11-13 - Keyman 20 planning; see [planning doc](https://docs.google.com/document/d/1c6P9pNQMlz-E4EfMTlaDkCuLDfwN_gmwTxgmaqo980E/edit?usp=sharing)
