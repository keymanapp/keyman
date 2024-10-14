# Keyman Developer

This is the current home for Keyman Developer.

## Build Prerequisites

* See [Windows Build Environment Configuration](../../docs/build/windows.md).

## Building Keyman Developer

1. Start 'Git Bash' (part of Git for Windows).
2. Run `developer/src/build.sh`.
3. Artifacts from a successful build will be placed in **developer/bin** folder.

*Note*: running `build.sh` will currently reset the packages and path settings
in your Delphi environment. If you use Delphi for other projects, you should
consider building Keyman under a login user dedicated to it, or in a VM.

Type `build.sh` to see build targets and actions. Common build actions are:

* `build.sh build`
: builds Keyman Developer

* `build.sh clean`
: remove temporary files and build artifacts

* `build.sh publish`
: makes a release of all Keyman Developer projects

* `build.sh install`
: install some or all components to Program Files (requires elevated command prompt).

### Building without Delphi

It is possible to build all components that do _not_ require Delphi. Currently a
few components are Delphi-based (TIKE, setup, a few others), but you may be able
to get away without building them. In this situation, we recommend copying the
relevant Delphi-built components into developer/bin folders from a compatible
installed version of Keyman Developer for testing and debugging purposes.

# Folders

## common

Units shared among Keyman Developer projects, Delphi

## ext

3rd party code and components, mostly Delphi

## inst - keymandeveloper.msi

Building and signing of the installation archive keymandeveloper.msi.

### inst/node

Contains a minimal distribution of node.js intended just for running kmc. If
more dependencies are required, then the developer will be expected to install
node.js themselves; this gets users going without requiring a large installer or
a full node.js install.

## kmanalyze - kmanalyze.exe

A keyboard source analysis and automated regression test tool. Testing for
logical errors of a keyboard.

## kmconvert - kmconvert.exe & kmconvert.x64.exe

KMConvert converts keyboard layouts between different formats. KMConvert will
generate a full Keyman template project from the imported layout or data, in a
new folder named with the ID of the keyboard. It will not overwrite an existing
folder. Also used to generate new projects from templates.

## kmdecomp - kmdecomp.exe

Unsupported utility decompiles a Keyman .kmx keyboard. It will produce a .kmn
source file and optionally a .ico or .bmp image. see kmdecomp/kmdecomp.md for
details.

## kmdbrowserhost - kmdbrowserhost.exe

Hosts Chromium components for Keyman Developer; similar to kmbrowserhost.exe
in Keyman for Windows.

## kmc

node-based next generation compiler, hosts kmc, (and legacy kmlmc, kmlmp)

### kmc-analyze - Analysis tools

File analysis tools for Keyman files.

### kmc-generate - Generation tools

Project generation tools for Keyman.

### kmc-keyboard-info - Keyboard Info Compiler

Builds .keyboard_info files for use on the Keyman Cloud keyboard repository
at https://github.com/keymanapp/keyboards. Command line access through kmc.

### kmc-kmn - Keyboard Compiler

Builds .kmx files from .kmn. Command line access through kmc.

### kmc-ldml - LDML Keyboard Compiler

Next Generation keyboard compiler - LDML keyboards. Command line access through
kmc.

### kmc-model - Lexical Model Compiler

The Lexical Model Compiler, runs on nodeJS on all supported desktop platforms.
Command line access through kmc.

### kmc-model-info - Model Info Compiler

Builds .model_info files for use on the Keyman Cloud lexical model repository at
https://github.com/keymanapp/lexical-models. Command line access through kmc.

### kmc-package - Package Compiler

Compiles .kps packages into .kmp files. Works with both lexical model packages
and keyboard packages. Command line access through kmc.

## samples

Contains sample keyboards and sample developer projects. See readme.txt for
details.

## server

Keyman Developer Server. Node-based, cross platform host app for Keyman
Developer. Used currently for web-based test and deployment. In future, will
be the foundation for a web-based Keyman Developer project.

## setup - setup.exe

Installation program for Keyman Developer for Windows. Bundles with
keymandeveloper.msi to generate single deployable installer executable.

## tike - "Tavultesoft Integrated Keyboard Editor"

This is the designer side of the Developer. Used for designing, developing,
testing and packing keyboards for distribution.

## tools

Various build-time tools for Keyman Developer.


