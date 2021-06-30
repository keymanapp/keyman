# Setup your Keyman build environment on macOS

## Target Projects

On macOS, you can build the following projects:

* Keyman for Android
* Keyman for iOS
* Keyman for macOS
* KeymanWeb

The following libraries can also be built:

* Common/Core/Desktop (macOS, wasm targets)
* Common/Core/Web

The following projects **cannot** be built on macOS:

* Keyman for Linux
* Keyman for Windows
* Keyman Developer

## System Requirements

* Minimum macOS version: macOS Catalina 10.15 or Big Sur 11.0

## Prerequisites

Many dependencies are only required for specific projects.

* XCode (iOS, macOS)
  * Install from App Store
  * Accept the Xcode license: `sudo xcodebuild -license accept`

The remaining dependencies can be installed via script:
  `resources/devbox/macos/macos.sh`

This script will also update your environment to the values in:
  `resources/devbox/macos/keyman.macos.env.sh`

It will also add these environment settings to your `~/.bashrc`.

These dependencies are also listed below if you'd prefer to install manually.

## Shared Dependencies

* Shared: HomeBrew, Bash 5.0+, jq, Python 3, Meson, Ninja, Rust, coreutils, Pandoc

  ```shell
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  brew install bash jq python3 meson ninja rustup-init coreutils pandoc rustup-init
  ```

* Web: node.js, emscripten, wasm-pack, openjdk 8

  ```shell
  brew install node emscripten wasm-pack openjdk@8
  rustup target add wasm32-unknown-unknown
  ```

* iOS: swiftlint, carthage

  ```shell
  brew install swiftlint carthage
  ```

* macOS: carthage, cocoapods

  ```shell
  brew install carthage cocoapods
  ```

* Android: openjdk 8, Android SDK, Android Studio, Ant, Gradle, Maven

  ```shell
  brew install openjdk@8 android-sdk android-studio ant gradle maven
  # update path
  source ../resources/devbox/macos/keyman.macos.env.sh
  # optionally install sdk images
  sdkmanager "system-images;android-30;google_apis;armeabi-v7a"
  sdkmanager --update
  sdkmanager --licenses
  ```

* kmcomp (optional): WINE
  - Required to build keyboards using kmcomp

  ```bash
  brew tap homebrew/cask-versions
  brew install --cask --no-quarantine wine-stable
  ```

* sentry-cli (optional)
  - Uploading symbols for Sentry-based error reporting

  ```
  brew install getsentry/tools/sentry-cli
  ```

* Run Android Studio once after installation to install additional components
  such as emulator images and SDK updates.
