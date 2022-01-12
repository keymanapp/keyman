# Setup your Keyman build environment on Ubuntu

## Target Projects

On Linux, you can build the following projects:

* [Keyman for Linux](#keyman-for-linux)
* [Keyman Core](#keyman-core) (Linux only) (aka common/core/desktop)

The following projects **cannot** be built on Linux because of outdated dependencies:

<!-- TODO: can we build Android, Web, Core-Wasm and Common/Web on Linux? -->
* Keyman for Android
* Keyman Core (wasm targets)
* Common/Web (aka common/core/web)
* KeymanWeb

* Keyman for Windows
* Keyman Developer
* Keyman for macOS
* Keyman for iOS

## System Requirements

* Minimum Ubuntu version: Ubuntu 18.04

Other Linux distributions will also work if appropriate dependencies are installed.

## Repository Paths

Recommended filesystem layout:

```text
$HOME/keyman/
  keyman/              this repository (https://github.com/keymanapp/keyman)
  keyboards/           https://github.com/keymanapp/keyboards
  sites/
    keyman.com/        https://github.com/keymanapp/keyman.com
    ...
```

## Prerequisites

The current list of dependencies can be found in the `Build-Depends` section of `linux/debian/control`.
They are most easily installed with the `mk-build-deps` tool:

```bash
sudo apt update
sudo apt install devscripts
sudo mk-build-deps --install linux/debian/control
```

## Keyman for Linux

All dependencies are already installed if you followed the instructions under [Prerequisites](#Prerequisites).

Building:

* [Building Keyman for Linux](../../linux/README.md)

## Keyman Core

All dependencies are already installed if you followed the instructions under [Prerequisites](#Prerequisites).

Building:

* [Building Keyman Core](../../common/core/desktop/doc/BUILDING.md)
