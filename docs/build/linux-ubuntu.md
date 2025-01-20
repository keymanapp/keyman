# Setup your Keyman build environment on Ubuntu

## Target Projects

On Linux, you can build the following projects:

- [Keyman Core](#keyman-core) (aka core)
- [Keyman for Linux](#keyman-for-linux)
- [Keyman Web](#keyman-web)
- [Keyman for Android](#keyman-for-android)
<!-- TODO: document how to build for Common/Web on Linux.
     See TC build agent for details. -->
- Common/Web

The following projects **cannot** be built on Linux:

- Keyman for Windows
- Keyman Developer
- Keyman for macOS
- Keyman for iOS

## Requirements

### System Requirements

- Minimum Ubuntu version: Ubuntu 20.04

Other Linux distributions will also work if appropriate dependencies are installed.

### Repository Paths

Recommended filesystem layout:

```text
$HOME/keyman/
  keyman/              this repository (https://github.com/keymanapp/keyman)
  keyboards/           https://github.com/keymanapp/keyboards
  sites/
    keyman.com/        https://github.com/keymanapp/keyman.com
    ...
```

### Prerequisites

The current list of dependencies can be found in the `Build-Depends` section of `linux/debian/control`.
They are most easily installed with the `mk-build-deps` tool:

```bash
sudo apt update
sudo apt install devscripts equivs
sudo mk-build-deps --install linux/debian/control
```

#### Node.js

Node.js is required for Core builds, Web builds, and Developer command line tool builds and usage.

Our recommended way to install node.js is with
[nvm](https://github.com/nvm-sh/nvm). This makes it easy to switch between
versions of node.js.

See [node.md](node.md) for more information.

#### Emscripten

You'll also have to install `emscripten`:

```shell
git clone https://github.com/emscripten-core/emsdk.git
cd emsdk
./emsdk install 3.1.58
./emsdk activate 3.1.58
cd upstream/emscripten
npm install
export EMSCRIPTEN_BASE="$(pwd)"
echo "export EMSCRIPTEN_BASE=\"$EMSCRIPTEN_BASE\"" >> .bashrc
```

If you are updating an existing install of Emscripten:

```bash
cd emsdk
git pull
./emsdk install 3.1.58
./emsdk activate 3.1.58
cd upstream/emscripten
npm install
```

> ![WARNING]
> Don't put EMSDK on the path, i.e. don't source `emsdk_env.sh`.
>
> Emscripten very unhelpfully overwrites `JAVA_HOME`, and adds its own
> versions of Python, Node and Java to the `PATH`. For best results, restart
> your shell after installing Emscripten so that you don't end up with the
> wrong versions.

**Optional environment variables**:

To let the Keyman build scripts control the version of Emscripten installed on
your computer:

```shell
export KEYMAN_USE_EMSDK=1
```

## Keyman Core

All dependencies are already installed if you followed the instructions under
[Prerequisites](#prerequisites).

### Building Keyman Core

Keyman Core can be built with the `core/build.sh` script.

- [Building Keyman Core](../../core/doc/BUILDING.md)

## Keyman for Linux

All dependencies are already installed if you followed the instructions
under [Prerequisites](#prerequisites).

### Building Keyman for Linux

Keyman for Linux can be built with the `linux/build.sh` script.

- [Building Keyman for Linux](../../linux/README.md)

## Keyman Web

Most dependencies are already installed if you followed the instructions under
[Prerequisites](#prerequisites). You'll still have to install Chrome:

```bash
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo apt install ./google-chrome-stable_current_amd64.deb
```

And add the `CHROME_BIN` environment variable to `.bashrc:

```bash
export CHROME_BIN=/opt/google/chrome/chrome
```

### Environment variables for Keyman Web

`CHROME_BIN` pointing to the Google Chrome binary.

### Building Keyman Web

Keyman Web can be built with the `web/build.sh` script.

- [Building Keyman Web](../../web/README.md)

## Keyman for Android

**Dependencies:**

Most dependencies are already installed if you followed the instructions
under [Prerequisites](#prerequisites).

**Additional requirements:**

- [Android Studio](https://developer.android.com/studio/install#linux)
  or sdkmanager
- Maven
- pandoc
- Android SDK
- Gradle
- jq

If you only use the command line you don't need Android Studio, however
to do development it's recommended to install it.

Run Android Studio once after installation to install additional components
such as emulator images and SDK updates.

Maven, jq and pandoc can be installed with:

```shell
sudo apt update
sudo apt install maven pandoc jq
```

If necessary, Android SDK and Gradle will be installed by the build script.
In order for that to work, run the following command once. You won't need
this if you install Android SDK through Android Studio.

```shell
sudo apt install sdkmanager
sudo sdkmanager platform-tools
sudo chown -R $USER:$USER /opt/android-sdk/
sdkmanager --licenses
```

### Environment variables for Keyman for Android

**Required environment variable:**

- `ANDROID_HOME` pointing to Android SDK (`$HOME/Android/Sdk`)

**Recommended environment variable:**

- [`JAVA_HOME`](#java_home)

### Building Keyman for Android

Keyman for Android can be built with the `android/build.sh` script.

- [Building Keyman for Android](../../android/README.md)

### Notes on Environment Variables

#### JAVA_HOME

This environment variable tells Gradle what version of Java to use for building
Keyman for Android. OpenJDK 11 is used for master.

It's recommended to set the environment variables to:

```bash
export JAVA_HOME="[path to OpenJDK 11]"
```

Also edit `/etc/profile.d/jvm.sh` as sudo:

```bash
export JAVA_HOME="[path to OpenJDK 11]"
```

**Multiple versions of Java:** If you need to build Keyman for Android 16.0 or
older versions, you can set `JAVA_HOME_11` to the OpenJDK 11 path and
`JAVA_HOME` to the OpenJDK 8 path. This will build both versions correctly
from command line. But note that you do need to update your `JAVA_HOME` env
var to the associated version before opening Android Studio and loading any
Android projects. `JAVA_HOME_11` is mostly used by CI.

## Docker Builder

The Docker builder allows you to perform a build from anywhere Docker is supported.
See [this README.md](../../resources/docker-images/README.md) for details.

### Using the builder with VSCode [Dev Containers](https://code.visualstudio.com/docs/devcontainers/tutorial)

1. Save the following as `.devcontainer/devcontainer.json`, updating the `image`
   to match the Docker image built above.

   ```json
   // file: .devcontainer/devcontainer.json
   {
           "name": "Keyman Ubuntu 23.04",
           "image": "keymanapp/keyman-linux-builder:u23.04-node18"
   }
   // For format details, see https://aka.ms/devcontainer.json. For config options,
   // see the README at: https://github.com/devcontainers/templates/tree/main/src/ubuntu
   ```

2. in VSCode, use the "Dev Containers: Open Folder In Container…" option and
   choose the Keyman directory.

3. You will be given a window which is running VSCode inside this builder
   image, regardless of your host OS.
