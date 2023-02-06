# Keyman engine for IBus

All commands to be run in `linux/ibus-keyman`.

Source code for Keyman engine for IBus is in [linux/ibus-keyman](../../linux/ibus-keyman/).

## Requirements

meson (>= 0.53)

## Building

```bash
./build.sh all
./build.sh test
sudo ./build.sh install
```

For a debug build:

```bash
CPPFLAGS=-DG_MESSAGES_DEBUG CFLAGS="-g -O0" CXXFLAGS="-g -O0" ./build.sh --debug clean config build
```

To use the header files from the source repo, you need to specify paths to
the include files in core:

```bash
CPPFLAGS="-DG_MESSAGES_DEBUG -I../../core/build/arch/debug/include/ \
  -I../../common/include/ -I../../core/include/" CFLAGS="-g -O0" \
CXXFLAGS="-g -O0" \./build.sh --debug clean config build
```
