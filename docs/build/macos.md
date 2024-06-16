# Setup your Keyman build environment on macOS

This document describes prerequisites and tools required for building various
Keyman projects on macOS. Each project will also have additional notes linked
below.

## Target Projects

On macOS, you can build the following projects:

* Keyman for Android ([additional details](../../android/README.md))
* Keyman for iOS ([additional details](../../ios/README.md))
* Keyman for macOS ([additional details](../../mac/README.md))
* KeymanWeb ([additional details](../../web/README.md))
* Keyman Developer Command Line Tools (kmc)

The following libraries can also be built:

* core (macOS, wasm targets)
* Common/Web

The following projects **cannot** be built on macOS:

* Keyman for Linux
* Keyman for Windows
* Keyman Developer (IDE component)

## System Requirements

* Minimum macOS version: macOS Catalina 10.15 or Big Sur 11.0

**Note:** to make a fully M1-compatible release build of Keyman for macOS (for
the setup Applescript), Big Sur 11.0 is required, as osacompile on earlier
versions does not build for arm64 (M1). The build will still work on earlier
versions, but the installer won't be able to run on M1 Macs that do not have
Rosetta 2 installed.

## Prerequisites

Many dependencies are only required for specific projects.

* XCode (iOS, macOS) 12.4 or later is needed only for Keyman for macOS and Keyman
  for iOS.
  * Install from App Store
  * Accept the Xcode license: `sudo xcodebuild -license accept`

The remaining dependencies can be installed via script:
  `resources/devbox/macos/macos.sh`

This script will also update your environment to the values in:
  `resources/devbox/macos/keyman.macos.env.sh`

It will also add these environment settings to your `~/.bashrc`.

These dependencies are also listed below if you'd prefer to install manually.

## Shared Dependencies

* HomeBrew, Bash 5.0+, jq, Python 2.7, Python 3, Meson, Ninja, coreutils, Pandoc

```shell
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew install bash jq python3 meson ninja coreutils pandoc pyenv
# Python 2.7 required for DeviceKit (among others?) at present
pyenv install 2.7.18
pyenv global 2.7.18
echo 'eval "$(pyenv init --path)"' >> ~/.bash_profile
```

On macOS, you will need to adjust your PATH so that coreutils’ `realpath` takes
precedence over the BSD one:

```shell
# Credit: brew info coreutils
PATH="$HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin:$PATH"
```

## KeymanWeb Dependencies

* node.js 18+, emscripten, openjdk 8

```shell
brew install node emscripten
```

Note: if you install emscripten with brew on macOS, only emscripten binaries are
added to the path via symlinks. This makes it reasonably safe to have emscripten
on the path, unlike on other platforms where emscripten also ends up adding its
versions of node, python, and other binaries to the path.

## Keyman for iOS Dependencies

* XCode, swiftlint, carthage

```shell
brew install swiftlint carthage
```

## Keyman for Mac Dependencies

* XCode, carthage, cocoapods

```shell
brew install carthage cocoapods
```

## Keyman for Android Dependencies

* openjdk 11, Android SDK, Android Studio, Ant, Gradle, Maven

```shell
brew install openjdk@11 android-sdk android-studio ant gradle maven
# update path
source ../resources/devbox/macos/keyman.macos.env.sh
# optionally install sdk images
sdkmanager "system-images;android-30;google_apis;armeabi-v7a"
sdkmanager --update
sdkmanager --licenses
```

* Note: Run Android Studio once after installation to install additional
components such as emulator images and SDK updates.

## Keyman Developer Command Line (kmc)

* node.js, emscripten

```shell
brew install node emscripten
```

## Optional Tools

* sentry-cli: Uploading symbols for Sentry-based error reporting

  ```shell
  brew install getsentry/tools/sentry-cli
  ```
