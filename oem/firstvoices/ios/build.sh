#!/bin/sh

set -e

# TODO: support passing -copy-keyboards, -debug, -clean etc in to build_keyboards
./build_keyboards.sh

TARGET=FirstVoices
# TODO: in the future build_common.sh should probably be shared with all oem products?
source build_common.sh
