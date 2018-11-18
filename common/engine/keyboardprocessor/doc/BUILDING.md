# How to build the keyboard processor

## Prerequisites
- Python 3
- Meson build system.
- C++14 or later compiler.
- lib std::fs

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
In your source directory do the following:
```
cd keyboardprocessor
meson build --werror
cd build
ninja
meson test
```
