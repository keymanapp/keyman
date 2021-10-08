# Setup your Keyman build environment on Windows

*** THIS FILE IS IN PROGRESS; see /windows/src/README.md for clearer notes.
* [interim notes](../../windows/src/README.md)

## Target Projects

On Windows, you can build the following projects:

* Keyman for Android
* Keyman for Windows
* Keyman Developer (together with Keyman for Windows)
* KeymanWeb

The following libraries can also be built:

* Keyman Core (Windows, wasm targets) (aka common/core/desktop)
* Common/Web (aka common/core/web)

The following projects **cannot** be built on Windows:

* Keyman for Linux
* Keyman for macOS
* Keyman for iOS

## System Requirements

* Minimum Windows version: Windows 10 x64

## Prerequisites

Many dependencies are only required for specific projects.

We prefer [Chocolatey](https://chocolatey.org/install) at present for
installation of dependencies.

Core development tools required:
* Delphi 10.3 Community or Professional:
  https://www.embarcadero.com/products/delphi/starter/free-download (Delphi
  Windows Community, DUnit Unit Testing Frameworks required)
* Visual C++ 2019 Community or Professional

  ```ps1
  choco install visualstudio2019community visualstudio2019-workload-nativedesktop visualstudio2019buildtools
  ```

<!--
The remaining dependencies can be installed via script:
  `resources/devbox/windows/windows.sh`

This script will also update your environment to the values in:
  `resources/devbox/windows/keyman.windows.env.sh`

It will also add these environment settings to your local profile?
-->

These dependencies are also listed below if you'd prefer to install manually.


While we use PowerShell for dependency installation, we use a mixture of bash
scripts and Borland makefiles to build.

## Shared Dependencies

* **Base**: git for Windows, jq, Python 3, Meson 0.56+, Ninja, Rust, Pandoc:

  ```ps1
  choco install git jq python ninja pandoc
  refreshenv
  # choco meson (0.55) is too old, 0.56 required:
  python -m pip install meson
  # choco rustup is not currently working
  # for *much* faster download, hide progress bar (PowerShell/PowerShell#2138)
  $ProgressPreference = 'SilentlyContinue'
  Invoke-WebRequest -Uri https://static.rust-lang.org/rustup/dist/x86_64-pc-windows-msvc/rustup-init.exe -OutFile $env:TEMP\rustup-init.exe
  & "$env:TEMP\rustup-init.exe"
  refreshenv
  rustup target add i686-pc-windows-msvc
  ```

* **Web**: node.js 14+, emscripten 2.0.23+, wasm-pack 0.9.1+, openjdk 8+

  ```ps1
  rustup target add wasm32-unknown-unknown
  choco install nodejs emscripten
  choco install openjdk

  # for *much* faster download, hide progress bar (PowerShell/PowerShell#2138)
  $ProgressPreference = 'SilentlyContinue'
  Invoke-WebRequest -Uri https://github.com/rustwasm/wasm-pack/releases/download/v0.9.1/wasm-pack-init.exe -OutFile $env:TEMP\wasm-pack-init.exe
  & "$env:TEMP\wasm-pack-init.exe"
  ```

## Android-only

* Requires: Base, Web
* **Android**: Android SDK, Android Studio, Ant, Gradle, Maven

  ```shell
  choco install android-sdk android-studio ant gradle maven
  # update path
  #source ../resources/devbox/windows/keyman.windows.env.sh
  # optionally install sdk images
  sdkmanager "system-images;android-30;google_apis;armeabi-v7a"
  sdkmanager --update
  sdkmanager --licenses
  ```

* Run Android Studio once after installation to install additional components
  such as emulator images and SDK updates.

## Optional Dependencies

* sentry-cli (optional)
  - Uploading symbols for Sentry-based error reporting

  ```bash
  curl -sL https://sentry.io/get-cli/ | bash
  ```

# Repository Paths

When cloning this repo for local development on a Windows machine, take care not
to place it overly deep in your file system. Some of the paths for compilation
can push character lengths around 160 characters long, while certain operations
on Windows systems may be limited to paths of 260 characters or less. For
example, [`git clean` on Windows with
msys](https://stackoverflow.com/questions/22575662/filename-too-long-in-git-for-windows/22575737#22575737)
is limited due to dependence on older Windows APIs.

# Environment Variables

```
KEYMAN_ROOT=<path to your keymanapp/keyman repo>

```

# Building