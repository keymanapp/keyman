# Keyman for Linux

## Projects

- [kmflcomp](./kmflcomp) - KMFL keyboard compiler
- [libkmfl](./libkmfl) - older KMFL core library
- [ibus-kmfl](./ibus-kmfl) - IBUS integration to use KMFL

See [license information](./LICENSE.md) about licensing.

## Linux Requirements/Setup

- It is helpful to be using the [packages.sil.org](http://packages.sil.org) repo

- Install packages required for building and developing KMFL

  ```bash
  sudo apt install cdbs debhelper libx11-dev autotools-dev build-essential \
    dh-autoreconf flex bison libibus-1.0-dev python3-setuptools meson \
    libjson-glib-dev libgtk-3-dev libxml2-utils help2man python3-lxml \
    python3-magic python3-numpy python3-pil python3-pip python3-qrcode \
    python3-requests python3-requests-cache python3 python3-gi dconf-cli \
    dconf-editor cargo python3-dbus
  ```

## Compiling from Command Line

### Build script

#### Installing for ibus to use ibus-kmfl

- The process to build and install everything is:

  - `make reconf` to create the build system and set the version
  - `make fullbuild` to configure and build
  - `sudo make install` to install to `/usr/local`

- Some of the files must be installed to `/usr/share/` so `make install` must be run as `sudo`.

  To do this run `sudo make install`

  - This will install to `/usr/local`
    - and `/usr/share/ibus/component/kmfl.xml` and `/usr/share/kmfl/icons`

  - If you already have the ibus-kmfl package installed then it will move the file `/usr/share/ibus/component/kmfl.xml` to `/usr/share/doc/ibus-kmfl/`

- run `sudo make uninstall` to uninstall everything and put it back again

#### Tmp install

Used by TC for validating PRs

Run `make tmpinstall` to build and install **keyboardprocessor**, **kmflcomp**, **libkmfl**, **ibus-kmfl** and **ibus-keyman** to `/tmp/kmfl`

This is only for testing the build, not for running **ibus-kmfl** in ibus

### Manually

- **libkmfl** requires `kmfl.h` header from **kmflcomp**
- **ibus-kfml** requires `kmfl.h` and headers and lib from **libkmfl**

So

- **kmflcomp** must be built and installed before **libkmfl**
- **libkmfl** must be built and installed before **ibus-kmfl**

For each project run `./configure && make && make install`.

You may prefer to create a different directory to build in and run configure from there e.g.

```bash
mkdir ../build-kmflcomp
cd ../build-kmflcomp
../kmflcomp/configure
make
make install
```

The install of **ibus-kmfl** doesn't install everything to the correct location for it to be
used - to be fixed
