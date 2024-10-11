#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder.inc.sh"
. "${THIS_SCRIPT%/*}/../../resources/build/meson-utils.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

################################ Main script ################################

builder_describe \
  "Mnemonic layout recompiler for macOS" \
  "@/common/include" \
  "clean" \
  "configure" \
  "build" \
  "test"

builder_parse "$@"

builder_describe_outputs \
  configure      build/build.ninja \
  build          build/mcompile

TARGET_PATH="$THIS_SCRIPT_PATH/build"

builder_run_action clean do_meson_clean
builder_run_action configure do_meson_configure
builder_run_action build do_meson_build
builder_run_action test do_meson_test
