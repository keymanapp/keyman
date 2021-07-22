# Common/Core/Desktop configuration notes

WARNING: these are old configuration notes. See [index.md](../index.md) for current steps.

- Bash (for Windows, included with Git for Windows)
- Python 3
- Meson build system 0.45+ (0.56+ for WASM)
- ninja 1.8+
- C++14 or later compiler (VC++ 2019 or later for Windows).
- lib std::fs
- kmcomp (for tests) -- must be added to path
- Rust 1.50+ from <https://www.rust-lang.org/tools/install> (or `cargo` package on Linux)

For WASM builds:
- Meson build system 0.56+
- emscripten 2.0.23+ <https://emscripten.org/docs/getting_started/downloads.html>
- WasmPack 0.9.1+ <https://github.com/rustwasm/wasm-pack>
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

* Install Rust:

  After downloading and running the executable from:
  <https://www.rust-lang.org/tools/install>

  Run the command:

  ```bash
  rustup target add i686-pc-windows-msvc
  rustup target add wasm32-unknown-unknown
  ```

* Install wasm-pack:
  <https://github.com/rustwasm/wasm-pack/releases/download/v0.9.1/wasm-pack-init.exe>

* Install Emscripten:
  <https://emscripten.org/docs/getting_started/downloads.html#sdk-download-and-install>

* Add emcc to PATH (probably upstream\enscripten)

#### Windows environment

On Windows you may need to set `SDKVER` environment variable to the current
Windows SDK version, if it cannot be automatically detected.

```bash
export SDKVER=10.0.19041.0
```

#### kmcomp

Note on paths for kmcomp:

The search path can be edited through System settings / Advanced system settings
/ Environment Variables / User environment variables.

If you have Keyman Developer installed, kmcomp should be on the path already;
otherwise add `%KeymanDeveloperPath%` to your path.

If you do not have Keyman Developer installed, add the path where you extracted
the kmcomp archive.

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

* Install Rust:

  ```bash
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
  rustup target add wasm32-unknown-unknown
  ```

* Install wasm-pack (WASM builds):

  ```bash
  curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
  ```

* Install Enscripten (including adding to path with `emsdk_env.sh`)  (WASM builds):
  <https://emscripten.org/docs/getting_started/downloads.html#sdk-download-and-install>

You may also need the `kmcomp` wrapper - see below.

#### Other Linux distributions

* Install Python 3 and pip

  You will be able to install a python3 package in any reputable recent version of
  linux using its package manager if it's not already installed. Ensure that `pip`
  is also installed.

* Install meson:

  ```bash
  python3 -m pip install meson
  ```

* Install Rust from <https://www.rust-lang.org/tools/install>, then:

  ```bash
  rustup target add wasm32-unknown-unknown
  ```

* Install wasm-pack (WASM builds):

  ```bash
  curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
  ```

* Install Enscripten (WASM builds):
  <https://emscripten.org/docs/getting_started/downloads.html#sdk-download-and-install>

* Add emcc to PATH (probably upstream/enscripten)

You may also need the `kmcomp` wrapper - see below.

#### kmcomp - All Linux platforms

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

* Install Python 3

  You can get the official Python installer from the official Python site:
  <https://www.python.org/downloads/mac-osx/>

* Install meson:

  ```bash
  brew install meson    # if you haven't already installed via pip
  ```

* Install Rust:

  ```bash
  curl https://sh.rustup.rs -sSf | sh
  rustup target add wasm32-unknown-unknown
  ```

* Install wasm-pack:

  ```bash
  curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
  ```

* Install Enscripten (including environment update):
  <https://emscripten.org/docs/getting_started/downloads.html#sdk-download-and-install>

#### kmcomp

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