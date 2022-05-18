# Keyman Developer for Windows

This is the current home for Keyman Developer.

# Transition steps

1. Split .kps and model compilers into separate paths

# Folders


## inst

Contains a minimal distribution of node.js intended just for compiling
lexical models. If more  dependencies are required, then the
developer will be expected to install node.js themselves; this gets
users going without requiring a large installer or a full node.js
install.

## kmlmc

Source files for node-based compilers kmlmc, kmlmi, and kmlmp

## inst - keymandeveloper.msi

Building and signing of the installation archive keymandeveloper.msi

## kmanalyze - kmanalyze.exe

A keyboard source analysis and automated regression test tool. Testing for logical errors of a keyboard.


## kcmpdll - kmcmpdll.dll & kmcmpdll.x64.dll

This is the main source code for the keyboard compiler, packaged as dynamic linked library. The consumers will call 'CompileKeyboardFile'

## kcframe.exe & kcframe.x64.exe

Located under kmcomp folder this is used for debugging in Visual Studio.  A command line program compiler for a keyboard file 'kmn' into a binary format 'kmx'. Has hardcoded default arguments.

## kmcomp - kmcomp.exe & kmcomp.x64.exe

The main command line compiler for a keyboard. It compiles keyboards (.kmn), packages (.kps), and projects (.kpj).

## kmconvert - kmconvert.exe & kmconvert.x64.exe

KMConvert converts keyboard layouts between different formats. KMConvert will generate a full Keyman template project from the imported layout or data, in a new folder named with the ID of the keyboard. It will not overwrite an existing folder.

## kmdecomp - kmdecomp.exe

Unsupported utility decompiles a Keyman .kmx keyboard. It will produce a .kmn source file and optionally a .ico or
.bmp image. see kmdecomp/kmdecomp.md for details.

## samples

Contains sample keyboards and sample developer projects. See readme.txt for details.

## setup - setup.exe

Installation program for Keyman Developer.

## TIKE - Tavultesoft Integrated Keyboard Editor

This is the designer side of the Developer. Used for designing, developing, testing and packing keyboards for distribution.

# kmlmc - Lexical Model Compiler

The Lexical Model Compiler, kmlmc, and the node-based
Package Compiler run on nodeJS on all supported desktop platforms.

## kmlmc/kmlmp - Package Compiler

The package compiler is broadly compatible with the kmcomp .kps
package compiler. However at this stage it is only tested with
lexical models, and use with keyboards (either .js or .kmx) is not
tested or supported. It is likely in the future that the kmcomp
.kps compiler will be deprecated in favour of this one.

## kmlmc/kmlmi - Model Info Compiler
