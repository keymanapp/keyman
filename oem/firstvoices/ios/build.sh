#!/bin/bash

set -e

export TARGET=FirstVoices
export KEYBOARDS_TARGET="$TARGET/Keyboards/files"
export KEYBOARDS_CSV_TARGET="$TARGET/Keyboards/keyboards.csv"

# TODO: support passing -copy-keyboards, -debug, -clean etc in to build_keyboards
../common/build_keyboards.sh -download-js-keyboards

TARGET=FirstVoices
# TODO: in the future build_common.sh should probably be shared with all oem products?
source build_common.sh
