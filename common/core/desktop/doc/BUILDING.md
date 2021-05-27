# How to build the keyboard processor

## Prerequisites

- Python 3
- Meson build system 0.50 or later.
- C++14 or later compiler (VC++ 2019 or later for Windows).
- Rust from https://www.rust-lang.org/tools/install
- lib std::fs
- kmcomp (for tests) -- must be added to path

### Windows

You can get the official Python installer from the official Python site:
<https://www.python.org/downloads/windows/>

Ensure you have Python3 correctly installed and can run the command `pip`.

Then install meson:

```bash
python3 -m pip install meson
```

Note on paths for kmcomp:

The search path can be edited through System settings / Advanced system settings
/ Environment Variables / User environment variables.

If you have Keyman Developer installed, add `%KeymanDeveloperPath%` to your
path. Otherwise, add the path where you extracted the kmcomp archive.

### Linux

You will be able to install a python3 package in any reputable recent version of
linux using its package manager if it's not already installed.

Then install meson:

```bash
sudo apt install python3 python3-pip
python3 -m pip install meson
```

If you want to rebuild keyboards for tests, you need a wrapper `kmcomp` shell
script:

```bash
#!/usr/bin/env bash
wine `dirname "$0"`/kmcomp.exe "$@"
```

Place this in the same folder as you extracted kmcomp.exe, and
`chmod +x kmcomp`. Add the folder to the path (e.g.
`export PATH=/path/to/kmcomp:$PATH`, which you can add to `.bashrc`).

### macOS

You can get the official installer from the official Python site:
<https://www.python.org/downloads/mac-osx/>

```
brew install meson    # if you haven't already installed via pip
```

If you want to rebuild keyboards for tests, you'll also need WINE:

```
brew tap homebrew/cask-versions
brew install --cask --no-quarantine wine-stable
```

And you will also need a wrapper `kmcomp` shell script:

```bash
#!/usr/bin/env bash
wine64 `dirname "$0"`/kmcomp.exe "$@"
```

Place this in the same folder as you extracted kmcomp.exe, and
`chmod +x kmcomp`. Add the folder to the path (e.g.
`export PATH=/path/to/kmcomp:$PATH`, which you can add to `.bashrc`).

## Building

### Building on Windows

For Windows, use `build.bat` -- this handles environment and x86/x64
cross-compiles with Visual Studio 2017+.

You may need to set `SDKVER` environement variable to the current
Windows SDK version, if it cannot be automatically detected, e.g.:

```DOS
set SDKVER=10.0.19041.0
```

To build:

```DOS
build.bat all
```

### Building on Linux, macOS

For all other platforms, in your source directory do the following:

```bash
cd desktop
meson build --werror
cd build
ninja
meson test
```

For a debug build, pass `--buildtype debug` to meson.

## Note on kmcomp

kmcomp is the command-line compiler from Keyman Developer, available from
<https://keyman.com/developer/download> or in this repo in
`/windows/src/developer/kmcomp`. The compiler is currently available as a
Windows PE executable only, but it does run under WINE.

