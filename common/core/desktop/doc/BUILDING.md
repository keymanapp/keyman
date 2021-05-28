# How to build the keyboard processor

## Prerequisites

- Python 3
- Meson build system 0.50 or later.
- C++14 or later compiler (VC++ 2019 or later for Windows).
- Rust from <https://www.rust-lang.org/tools/install> (or `cargo` package on Linux)
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

#### Ubuntu and Debian

You can install `meson` and Rust through the package manager:

```bash
sudo apt update
sudo apt install meson cargo
```

You'll also need the `kmcomp` wrapper - see below.

#### Other Linux distributions

You will be able to install a python3 package in any reputable recent version of
linux using its package manager if it's not already installed.

Then install meson:

```bash
sudo apt install python3 python3-pip
python3 -m pip install meson
```

Install Rust from <https://www.rust-lang.org/tools/install>.

You'll also need the `kmcomp` wrapper - see below.

#### All Linux platforms

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

```bash
brew install meson    # if you haven't already installed via pip
```

If you want to rebuild keyboards for tests, you'll also need WINE:

```bash
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

On Windows you may need to set `SDKVER` environement variable to the current
Windows SDK version, if it cannot be automatically detected, e.g.:

```DOS
set SDKVER=10.0.19041.0
```

### Building on all platforms

On all other platforms, use `build.sh`.

```bash
./build.sh --debug
```

## Note on kmcomp

kmcomp is the command-line compiler from Keyman Developer, available from
<https://keyman.com/developer/download> or in this repo in
`/windows/src/developer/kmcomp`. The compiler is currently available as a
Windows PE executable only, but it does run under WINE.

