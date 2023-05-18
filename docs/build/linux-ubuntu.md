# Setup your Keyman build environment on Ubuntu

## Target Projects

On Linux, you can build the following projects:

* [Keyman for Linux](#keyman-for-linux)
* [Keyman Core](#keyman-core) (Linux only) (aka core)
* [Keyman for Android](#keyman-for-android)
<!-- TODO: document how to build for Web, Core-Wasm and Common/Web on Linux. See TC build agent for details. -->
* Keyman Core (wasm targets)
* Common/Web
* KeymanWeb

The following projects **cannot** be built on Linux:

* Keyman for Windows
* Keyman Developer
* Keyman for macOS
* Keyman for iOS

## System Requirements

* Minimum Ubuntu version: Ubuntu 20.04

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

- core

```shell
# build 'core' in docker
cd ../core
# keep linux build artifacts separate
mkdir -p build/linux
docker run -it --rm -v $(pwd)/..:/home/build -v $(pwd)/build/linux:/home/build/core/build keyman-linux-builder:latest bash -c 'cd core; bash build.sh -d'
```

- linux

```shell
# build 'linux' installation in docker
# runs as root!
# note: run 'git clean -fdx linux' afterwards to cleanup
cd keymanapp/keyman
docker run -it --rm -v $(pwd):/home/build/src/keyman -u root -w /home/build/src/keyman keymanapp/keyman-linux-builder:latest bash -c "cd linux && make reconf && make fullbuild && make install"
```

## Keyman for Android

**Dependencies**:
* [Base](#base-dependencies)
* [Web](./windows#web-dependencies)

**Additional requirements**:
* Android SDK
* [Android Studio](https://developer.android.com/studio/install#linux)
* Gradle
* Maven
* OpenJDK 11 (for Keyman 17.0+)


Run Android Studio once after installation to install additional components
such as emulator images and SDK updates.

**Required environment variables**:
* [`JAVA_HOME`](#java_home)

Building:
* [Building Keyman for Android](../../android/README.md)

## Prerequisites

Many dependencies are only required for specific projects.

### Base Dependencies

**Environment variables**:


## Notes on Environment Variables

### JAVA_HOME

This environment variable tells Gradle what version of Java to use for building Keyman for Android.
OpenJDK 11 is used for master. It's recommended to set the environment variables to:
```bash
EXPORT JAVA_HOME="[path to OpenJDK 11]"
```

Also edit `/etc/profile.d/jvm.sh` as sudo:
```bash
EXPORT JAVA_HOME="[path to OpenJDK 11]"
```
