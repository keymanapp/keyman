#!/bin/bash

set -e
# set -x

export TARGET=FirstVoices
export KEYBOARDS_TARGET=app/src/main/assets/packages
export KEYBOARDS_CSV_TARGET=app/src/main/assets/keyboards.csv

# TODO: support passing -copy-keyboards, -debug, -clean etc in to build_keyboards
./build_keyboards.sh

# TODO: in the future build_common.sh should probably be shared with all oem products?
./build_common.sh "$@"
