# Keyman engine for IBus

All commands to be run in `linux/ibus-keyman`.

Source code for Keyman engine for IBus is in [linux/ibus-keyman](../../linux/ibus-keyman/).

## Requirements

You need autoconf, autopoint, gettext, automake and libtool to generate the build system.

Run `./autogen.sh` to run them.

## Building

```bash
./autogen.sh
./configure
make
sudo make install
```

For a debug build:

```bash
./configure CPPFLAGS=-DG_MESSAGES_DEBUG CFLAGS="-g -O0" CXXFLAGS="-g -O0"
```

To use the header files from the source repo, you need to specify paths to the include files in core:

```bash
./configure CPPFLAGS="-DG_MESSAGES_DEBUG -I../../core/build/arch/debug/include/ -I../../common/include/ -I../../core/include/" \
  CFLAGS="-g -O0" CXXFLAGS="-g -O0"
```
