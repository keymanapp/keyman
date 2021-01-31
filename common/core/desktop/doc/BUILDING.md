# How to build the keyboard processor

## Prerequisites
- Python 3
- Meson build system.
- C++14 or later compiler (VC++ 2017 or later for Windows).
- lib std::fs
- kmcomp (for tests) -- must be added to path

## Installing Python3
### Linux
You will be able to install a python3 package in any reputable recent version
of linux using its package manager if it's not already installed.

### Mac OS X
You can get the official installer from the official Python site:
[https://www.python.org/downloads/mac-osx/]()

### Windows
You can get the official installer from the official Python site:
[https://www.python.org/downloads/windows/]()


## Installing Meson
Ensure you have Python3 correctly installed and can run the command `pip3`.
```
python3 -m pip install meson
```

## Building

### Building on Windows
For Windows, use `build.bat` -- this handles environment and x86/x64
cross-compiles with Visual Studio 2017+.

You may need to set `SDKVER` environement variable to the current
Windows SDK version, if it cannot be automatically detected, e.g.:

```
set SDKVER=10.0.19041.0
```

To build:
```
build.bat all
```

### Building on Linux, macOS
For all other platforms, in your source directory do the following:
```
cd desktop
meson build --werror
cd build
ninja
meson test
```

## Note on kmcomp
kmcomp is the command-line compiler from Keyman Developer, available from https://keyman.com/ or
in this repo in /windows/src/developer/kmcomp. The compiler is currently available as a Windows
PE executable only, but it does run under WINE.

## Additional configuration notes

### Windows
The search path can be edited through System settings / Advanced system settings /
Environment Variables / User environment variables.

If you have Keyman Developer installed, add %KeymanDeveloperPath% to your path. Otherwise, add
the path where you extracted the kmcomp archive.

### Linux
You need a wrapper `kmcomp` shell script:

```
#!/bin/bash
wine `dirname "$0"`/kmcomp.exe "$@"
```

Place this in the same folder as you extracted kmcomp.exe, and `chmod +x kmcomp`. Add the folder
to the path (e.g. `export PATH=/path/to/kmcomp:$PATH`, which you can add to `.bashrc`)

### macOS
TODO
