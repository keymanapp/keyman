# Setup your Keyman build environment on Windows

## Target Projects

On Windows, you can build the following projects:

* [Keyman for Android](#keyman-for-android)
* [Keyman for Windows](#keyman-for-windows)
* [Keyman Developer](#keyman-developer) (together with Keyman for Windows)
* [KeymanWeb](#keymanweb)

The following libraries can also be built:

* Keyman Core (Windows, wasm targets) (aka core)
* Common/Web

The following projects **cannot** be built on Windows:

* Keyman for Linux
* Keyman for macOS
* Keyman for iOS

## System Requirements

* Minimum Windows version: Windows 10 x64

## Repository Paths

When cloning this repo for local development on a Windows machine, take care not
to place it overly deep in your file system. Some of the paths for compilation
can push character lengths around 160 characters long, while certain operations
on Windows systems may be limited to paths of 260 characters or less. For
example, [`git clean` on Windows with
msys](https://stackoverflow.com/questions/22575662/filename-too-long-in-git-for-windows/22575737#22575737)
is limited due to dependence on older Windows APIs.

Recommended filesystem layout:

```
C:\Projects\keyman\
  keyman\              this repository (https://github.com/keymanapp/keyman)
  keyboards\           https://github.com/keymanapp/keyboards
  lexical-models\      https://github.com/keymanapp/lexical-models
  CEF4Delphi_Binary\   https://github.com/keymanapp/CEF4Delphi_Binary
  sites\
    keyman.com\        https://github.com/keymanapp/keyman.com
    ...
```

Instructions and scripts in this file assume this layout; if you use a different
layout, adjust the paths accordingly.

We recommend adding an exclusion for C:\Projects to your antivirus/security
software, for performance reasons; this can also avoid build failures when
security software locks generated executables to scan them, before the build has
finished with them.

## Project Requirements

### Keyman for Windows

Dependencies:
* [Base](#base-dependencies)
* [Windows Platform](#windows-platform-dependencies)

Building:
* [Building Keyman for Windows](../../windows/src/README.md)

### Keyman Developer

Dependencies:
* [Base](#base-dependencies)
* [Web](#web-dependencies)
* [Windows Platform](#windows-platform-dependencies) (optional, for Windows-only components)

Building:
* [Building Keyman Developer](../../windows/src/README.md)

### KeymanWeb

Dependencies:
* [Base](#base-dependencies)
* [Web](#web-dependencies)

Building:
* [Building KeymanWeb](../../web/README.md)

### Keyman for Android

**Dependencies**:
* [Base](#base-dependencies)
* [Web](#web-dependencies)
* [Android](#android-dependencies)

Building:
* [Building Keyman for Android](../../android/README.md)

---

## Dependencies and Prerequisites

Many dependencies are only required for specific projects.

We prefer [Chocolatey](https://chocolatey.org/install) at present for
installation of most dependencies. Chocolatey should be run in an elevated
PowerShell.

### Base Dependencies

**Projects**:
* all projects

**Requirements**:
* git for Windows
* jq
* Python 3
* Meson 1.0+
* Ninja
* Pandoc

```ps1
# Elevated PowerShell
# for *much* faster download, hide progress bar (PowerShell/PowerShell#2138)
$ProgressPreference = 'SilentlyContinue'
choco install git jq python ninja pandoc meson
refreshenv
```

**Environment variables**:
* [`KEYMAN_ROOT`](#keyman_root)

```bat
SET KEYMAN_ROOT=c:\Projects\keyman\keyman
```

To check whether environment variables are set, run `SET <variable>` in command
prompt.

You can use Windows Settings to add these environment variables permanently:

1. In Windows Search, type "environment" and select "Edit System Environment
   Variables"
2. Click `Environment Variables...`
3. You can add or edit variables in either User or System settings, as you
   prefer.

### Web Dependencies

**Projects**:
* Keyman Developer
* Keyman for Android
* KeymanWeb

**Requirements**:
* Emscripten
* node.js

#### Emscripten

In an appropriate folder, e.g. `/c/Projects/keyman/`, in bash, run the following commands:

```bash
git clone https://github.com/emscripten-core/emsdk
cd emsdk
emsdk install 3.1.58
emsdk activate 3.1.58
```

Note: emscripten very unhelpfully overwrites JAVA_HOME, and adds its own
versions of Python, Node and Java to the PATH. For best results, restart
your shell after installing Emscripten so that you don't end up with the
wrong versions.

There is no need to add emscripten to the path in order to build Keyman.
However, you should set the EMSCRIPTEN_BASE variable to the path where `emcc`
can be found, but always in the upstream\emscripten subdirectory where you
installed emsdk.

**Environment variables**:
* `EMSCRIPTEN_BASE`: `<your-emsdk-path>\upstream\emscripten`

**Optional environment variables**:
* `KEYMAN_USE_EMSDK`: `1` to let the Keyman build scripts control the
  version of Emscripten installed on your computer.

After installing emscripten, you'll need to install node.js and openjdk.

#### node.js

Our recommended way to install node.js is to use
[nvm-windows](https://github.com/coreybutler/nvm-windows). This makes it
easy to switch between versions of node.js.

```
nvm install 20.16.0
nvm use 20.16.0
```

**Optional environment variables**:
* `KEYMAN_USE_NVM`: `1` to let the Keyman build scripts control the
  version of node.js installed and active on your computer.

See [node.md](node.md) for more information, including automatic selection
of appropriate node versions during builds.

### Windows Platform Dependencies

**Projects**:
* Keyman Developer
* Keyman for Windows

**Requirements**:
* Delphi 10.3 Community or Professional:
  https://www.embarcadero.com/products/delphi/starter/free-download (Delphi
  Windows Community, DUnit Unit Testing Frameworks required)

  * Note: Delphi 10.3 Community is no longer available. Delphi 10.4 Community no
    longer includes command line compilers. This change means that building
    Keyman with Delphi 10.4 Community is not really viable. This means you can
    only really use the Professional Edition, which can be used on a trial basis
    for a short time. (We are actively working to remove Delphi dependencies
    given the licensing issues with using it.)

  Start Delphi IDE once after installation as it will create various environment
  files and take you through required registration.

  * Note: It is possible to build all components that do _not_ require Delphi.
    Currently many components are Delphi-based, but if you are working just in
    Keyman Core, the compiler, or Keyman Engine's C++ components, you may be
    able to get away without building them. In this situation, we recommend
    copying the relevant Delphi-built components into windows/bin folders from a
    compatible installed version of Keyman for testing and debugging purposes.

* Visual C++ 2019 Community or Professional

  ```ps1
  choco install visualstudio2019community visualstudio2019-workload-nativedesktop visualstudio2019buildtools
  ```
  * Verify required build tools are installed
    * Run `Visual Studio Installer`
    * Check the `Individual components` tab
    * Verify `MSVC v142 - VS 2019 c++ x64/x86 build tools (Latest)` is installed. If not, install it.

  Recommended: configure Visual Studio to use two-space tab stops:
  1. Open the options dialog: Tools > Options.
  2. Navigate to Text Editor > All Languages > Tabs.
  3. Change 'Tab size' to 2 and 'Indent size' to 2.
  4. Select 'Insert spaces'.

**Required environment variables**:
* `PATH`
  * Add the C:\Projects\keyman\keyman\windows\lib folder in the Keyman
    repository to your `PATH` environment variable. This is required for
    Keyman's design-time packages to load in Delphi.
* [`KEYMAN_CEF4DELPHI_ROOT`](#keyman_cef4delphi_root)

```bat
SET KEYMAN_CEF4DELPHI_ROOT=c:\Projects\keyman\CEF4Delphi_Binary
```

**Additional requirements for release builds**:
* [Certificates](#certificates)
* [7-Zip](http://www.7-zip.org/), used for archiving build files
* [HTML Help Workshop](http://web.archive.org/web/20160201063255/http://download.microsoft.com/download/0/A/9/0A939EF6-E31C-430F-A3DF-DFAE7960D564/htmlhelp.exe) note: Microsoft no longer offer this download...
* [WiX 3.11.1](https://github.com/wixtoolset/wix3/releases/tag/wix3111rtm)
* [CEF4Delphi_Binary](https://github.com/keymanapp/CEF4Delphi_Binary) repository

```ps1
# Elevated PowerShell
choco install 7zip html-help-workshop
choco install wixtoolset --version=3.11.1
git clone https://github.com/keymanapp/CEF4Delphi_Binary C:\Projects\keyman\CEF4Delphi_Binary
```

### Android dependencies

**Projects**:
* Keyman for Android

**Requirements**:
* Android SDK
* Android Studio
* Ant
* Gradle
* Maven
* Optional: OpenJDK 11 (https://learn.microsoft.com/en-us/java/openjdk/download)

```ps1
# Elevated PowerShell
choco install android-sdk android-studio ant gradle maven
# optionally install sdk images
sdkmanager "system-images;android-33;google_apis;armeabi-v7a"
sdkmanager --update
sdkmanager --licenses
```

#### openjdk

Use Powershell + Chocolatey to install OpenJDK:

```ps1
# Elevated PowerShell

# for *much* faster download, hide progress bar (PowerShell/PowerShell#2138)
$ProgressPreference = 'SilentlyContinue'
choco install openjdk
```

* Run Android Studio once after installation to install additional components
  such as emulator images and SDK updates.

**Required environment variables**:
* [`JAVA_HOME`](#java_home)

## Certificates

In order to make a release build, you need to sign all the executables. See
[windows/src/README.md#Certificates](../../windows/src/README.md#Certificates)
for details on how to create test code signing certificates or specify your own
certificates for the build.

## Notes on Environment Variables

### KEYMAN_ROOT

If you pull the entire `keyman.git` repo to `c:\keyman`, then the paths by
default will work without changes. Otherwise, you will need to set an
environment variable `KEYMAN_ROOT` to the root path of the Keyman repo. For
example:

```bat
SET KEYMAN_ROOT=c:\projects\keyman\keyman
```

### KEYMAN_CEF4DELPHI_ROOT

Keyman and Keyman Developer use Chromium Embedded Framework. The source repo is
at https://github.com/keymanapp/CEF4Delphi. In order to build the installers, we
need to source the binary files from the
https://github.com/keymanapp/CEF4Delphi_binary repo. The
`KEYMAN_CEF4DELPHI_ROOT` environment variable should be set to the root of this
repo on your local machine.

The version of CEF in use is determined by CEF_VERSION.md. This maps to a branch
prefixed with `v` e.g. `v89.0.18` in the CEF4Delphi_binary repository. During a
release build, the common/windows/cef-checkout.sh script will checkout the correct
branch of the repository automatically and extract any compressed files found in
it.

### JAVA_HOME

This environment variable tells Gradle what version of Java to use for building Keyman for Android.

## Optional Tools

* sentry-cli (optional)
  - Uploading symbols for Sentry-based error reporting

  bash:
  ```bash
  # bash
  curl -sL https://sentry.io/get-cli/ | bash
  ```
