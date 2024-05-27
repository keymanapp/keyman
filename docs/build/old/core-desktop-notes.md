# Core configuration notes

WARNING: these are old configuration notes. See [index.md](../index.md) for current steps.

- Bash (for Windows, included with Git for Windows)
- Python 3
- Meson build system 0.45+ (0.56+ for WASM)
- ninja 1.8+
- C++17 or later compiler (VC++ 2019 or later for Windows).
- lib std::fs
- kmc (for tests) -- must be added to path

For WASM builds:
- Meson build system 0.56+
- emscripten 2.0.23+ <https://emscripten.org/docs/getting_started/downloads.html>
- ninja 1.10+ for WASM

### Windows

* Install Git for Windows: <https://gitforwindows.org/>

* Install Python 3:

  You can get the official Python installer from the official Python site:
  <https://www.python.org/downloads/windows/>

  Ensure you have Python 3 correctly installed and can run the command `pip` or
  `python -m pip`.

  Ensure that Python 3 is on the path ahead of Python 2 (if installed):
  `python --version` should return `Python 3.9.1` or similar.

* Install meson:

  ```bash
  python -m pip install meson
  ```

* Install [ninja](https://ninja-build.org/) 1.10+:

  ```bash
  choco install ninja
  ```

* Install Emscripten:
  <https://emscripten.org/docs/getting_started/downloads.html#sdk-download-and-install>

* Add emcc to PATH (probably upstream\enscripten)

#### Windows environment

On Windows you may need to set `SDKVER` environment variable to the current
Windows SDK version, if it cannot be automatically detected.

```bash
export SDKVER=10.0.19041.0
```

### Linux

#### Ubuntu and Debian

* Install Python

  ```bash
  sudo apt install python3
  ```

* Upgrade Meson from version included with Python (WASM builds):

  ```bash
  sudo apt install meson
  ```

* Install Enscripten (including adding to path with `emsdk_env.sh`)  (WASM builds):
  <https://emscripten.org/docs/getting_started/downloads.html#sdk-download-and-install>

You may also need `kmc` - see below.

#### Other Linux distributions

* Install Python 3 and pip

  You will be able to install a python3 package in any reputable recent version of
  linux using its package manager if it's not already installed. Ensure that `pip`
  is also installed.

* Install meson:

  ```bash
  python3 -m pip install meson
  ```

* Install Enscripten (WASM builds):
  <https://emscripten.org/docs/getting_started/downloads.html#sdk-download-and-install>

* Add emcc to PATH (probably upstream/enscripten)

You may also need `kmc` - see below.

#### kmc - All Linux platforms

If you want to rebuild keyboards for tests, you need to install `kmc`:

```bash
npm i -g @keymanapp/kmc
```

### macOS

* Install Python 3

  You can get the official Python installer from the official Python site:
  <https://www.python.org/downloads/mac-osx/>

* Install meson:

  ```bash
  brew install meson    # if you haven't already installed via pip
  ```

* Install Enscripten (including environment update):
  <https://emscripten.org/docs/getting_started/downloads.html#sdk-download-and-install>

#### kmc

If you want to rebuild keyboards for tests, you'll also need node:

```bash
brew install node
```

And you will also need to install `kmc`:

```bash
npm install -g @keymanapp/kmc
```

## Building -- all platforms

On all platforms, use `build.sh`.

* To build native libraries and tests:

  ```bash
  ./build.sh --debug
  ```

* To build WASM libraries and tests:

  ```bash
  ./build.sh --platform wasm
  ```
