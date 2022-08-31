# Setup your Keyman build environment on Ubuntu

## Target Projects

On Linux, you can build the following projects:

* [Keyman for Linux](#keyman-for-linux)
* [Keyman Core](#keyman-core) (Linux only) (aka core)
<!-- TODO: document how to build for Android, Web, Core-Wasm and Common/Web on Linux. See TC build agent for details. -->
* Keyman for Android
* Keyman Core (wasm targets)
* Common/Web
* KeymanWeb

The following projects **cannot** be built on Linux:

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
sudo apt install devscripts equivs
sudo mk-build-deps --install linux/debian/control
```

## Keyman for Linux

All dependencies are already installed if you followed the instructions under [Prerequisites](#Prerequisites).

Building:

* [Building Keyman for Linux](../../linux/README.md)

## Keyman Core

All dependencies are already installed if you followed the instructions under [Prerequisites](#Prerequisites).

Building:

* [Building Keyman Core](../../core/doc/BUILDING.md)

## Docker Builder

The Docker builder allows you to perform a linux build from anywhere Docker is supported.
To build the docker image:

```shell
cd ../../linux
docker pull ubuntu:latest
docker build . -t keymanapp/keyman-linux-builder:latest
```

Once the image is built, it may be used to build parts of Keyman.

```shell
cd ../core
# keep linux build artifacts separate
mkdir -p build/linux
docker run -it --rm -v $(pwd)/..:/home/build -v $(pwd)/build/linux:/home/build/core/build keyman-linux-builder:latest bash -c 'cd core; bash build.sh -d'
```
