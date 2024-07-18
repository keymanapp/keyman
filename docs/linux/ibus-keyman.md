# Keyman engine for IBus

All commands to be run in `linux/ibus-keyman`.

Source code for Keyman engine for IBus is in [linux/ibus-keyman](../../linux/ibus-keyman/).

## Requirements

meson (>= 1.0)

## Building

```bash
./build.sh clean configure build
./build.sh test
sudo ./build.sh install
```

For a debug build:

```bash
./build.sh --debug clean configure build
```
