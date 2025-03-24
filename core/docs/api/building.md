---
title: How to build Keyman Core
---

## Prerequisites

### To build

*   Python 3
*   Meson build system.
*   C++17 or later compiler.

### Optional

*   [kmc](https://keyman.com/developer/download) (for testing)

## Installing Python3

### Linux

You will be able to install a python3 package in any reputable recent version of
linux using its package manager if it's not already installed.

### macOS

You can get the official installer from the official Python site:
[https://www.python.org/downloads/mac-osx](https://www.python.org/downloads/mac-osx/)

### Windows

You can get the official installer from the official Python site:
[https://www.python.org/downloads/windows](https://www.python.org/downloads/windows/)

## Installing Meson

Ensure you have Python3 correctly installed and can run the command `pip3`.

    $> python3 -m pip install meson

## Building

In your source directory do the following:

    $> cd core
    $> ./build.sh configure build test

## Note on kmc

kmc is node.js-based the command-line compiler from Keyman Developer, available
from [keyman.com](https://keyman.com/developer/) or on npm at
[@keymanapp/kmc](https://npmjs.com/package/@keymanapp/kmc).

### Windows

The search path can be edited through System settings / Advanced system settings
/ Environment Variables / User environment variables.

If you have Keyman Developer installed, kmc should already be on your path.
Otherwise, add the path where you extracted the kmcomp archive.

### Linux & MacOS

Install kmc from the NPM package.
