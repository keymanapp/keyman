# Keyman engine for IBus

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
