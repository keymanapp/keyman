# Keyman Developer

This is the current home for Keyman Developer.

# Cross-dependencies

We do not have 100% clean separation between Keyman Developer and Keyman for
Windows. Shared units are intended to live in common, however the following
projects will need to be updated in the future.

* buildpkg in Windows depends on various compiler units in multiple folders.
  -- should be removed as part of 17.0.

* kmbrowserhost in Windows is included as part of Keyman Developer.

# Folders

## src/common

Units shared among Keyman Developer projects, Delphi

## src/ext

3rd party code and components, mostly Delphi

## src/inst - keymandeveloper.msi

Building and signing of the installation archive keymandeveloper.msi.

### src/inst/node

Contains a minimal distribution of node.js intended just for running kmc. If
more dependencies are required, then the developer will be expected to install
node.js themselves; this gets users going without requiring a large installer or
a full node.js install.

## src/kmanalyze - kmanalyze.exe

A keyboard source analysis and automated regression test tool. Testing for
logical errors of a keyboard.

## src/kmcmpdll - kmcmpdll.dll & kmcmpdll.x64.dll

This is the main source code for the keyboard compiler, packaged as a dynamic
linked library for Windows. The consumers will call 'CompileKeyboardFile'.

### src/kmcmpdll/kcframe - kcframe.exe & kcframe.x64.exe

Located under kmcmpdll folder this is used for debugging in Visual Studio.  A
command line program compiler for a keyboard file 'kmn' into a binary format
'kmx'. Has hardcoded default arguments.

## src/kmcomp - kmcomp.exe & kmcomp.x64.exe

The main command line compiler for a keyboard. It compiles keyboards (.kmn),
packages (.kps), and projects (.kpj).

## src/kmconvert - kmconvert.exe & kmconvert.x64.exe

KMConvert converts keyboard layouts between different formats. KMConvert will
generate a full Keyman template project from the imported layout or data, in a
new folder named with the ID of the keyboard. It will not overwrite an existing
folder. Also used to generate new projects from templates.

## src/kmdecomp - kmdecomp.exe

Unsupported utility decompiles a Keyman .kmx keyboard. It will produce a .kmn
source file and optionally a .ico or .bmp image. see kmdecomp/kmdecomp.md for
details.

## src/kmc

node-based next generation compiler, hosts kmc, kmlmi, kmlmc, kmlmp

### src/kmc-ldml - LDML Keyboard Compiler

Next Generation keyboard compiler package - LDML keyboards only at present.
Command line access through kmc.

### src/kmc-model - Lexical Model Compiler

The Lexical Model Compiler, kmlmc, runs on nodeJS on all supported desktop
platforms. Command line access through kmc/kmlmc.

### src/kmc-package - Package Compiler

The package compiler is broadly compatible with the kmcomp .kps package
compiler. However at this stage it is only tested with lexical models, and use
with keyboards (either .js or .kmx) is not tested or supported. It is likely in
the future that the kmcomp .kps compiler will be deprecated in favour of this
one. Command line access through kmc/kmlmp.

### src/kmc-model-info - Model Info Compiler

Merges .model_info files for use on the Keyman Cloud lexical model repository at
https://github.com/keymanapp/lexical-models. Command line access through
kmc/kmlmi.

## src/samples

Contains sample keyboards and sample developer projects. See readme.txt for
details.

## src/server

Keyman Developer Server. Node-based, cross platform host app for Keyman
Developer. Used currently for web-based test and deployment. In future, will
be the foundation for a web-based Keyman Developer project.

## src/setup - setup.exe

Installation program for Keyman Developer for Windows. Bundles with
keymandeveloper.msi to generate single deployable installer executable.

## src/tike - "Tavultesoft Integrated Keyboard Editor"

This is the designer side of the Developer. Used for designing, developing,
testing and packing keyboards for distribution.

## src/tools

Various build-time tools for Keyman Developer.