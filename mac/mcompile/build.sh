#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder.inc.sh"
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

do_clean() {
  rm -rf "$THIS_SCRIPT_PATH/resources"
  rm -rf "$TARGET_PATH"
}

do_configure() {
  # Import our standard compiler defines; this is copied from
  # /resources/build/meson/standard.meson.build by build.sh, because meson doesn't
  # allow us to reference a file outside its root
  mkdir -p "$THIS_SCRIPT_PATH/resources"
  cp "$KEYMAN_ROOT/resources/build/meson/standard.meson.build" "$THIS_SCRIPT_PATH/resources/meson.build"

  pushd "$THIS_SCRIPT_PATH" > /dev/null
  # Additional arguments are used by Linux build, e.g. -Dprefix=${INSTALLDIR}
  meson setup build --buildtype $BUILDER_CONFIGURATION "${builder_extra_params[@]}"
  popd > /dev/null

}

do_build() {
  pushd "$TARGET_PATH" > /dev/null
  ninja
  popd > /dev/null
}

do_test() {
  pushd "$TARGET_PATH" > /dev/null
  meson test "${builder_extra_params[@]}"
  popd > /dev/null
}

builder_run_action clean do_clean
builder_run_action configure do_configure
builder_run_action build do_build
builder_run_action test do_test