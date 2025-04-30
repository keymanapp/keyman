#!/usr/bin/env bash

# This script contains common meson functions

source "$KEYMAN_ROOT/resources/build/meson/standard_meson_build.inc.sh"

# ----------------------------------------------------------------------------
# clean
# ----------------------------------------------------------------------------

do_meson_clean() {
  rm -rf "$THIS_SCRIPT_PATH/resources"
  rm -rf "$TARGET_PATH"
}

# ----------------------------------------------------------------------------
# configure
# ----------------------------------------------------------------------------

do_meson_configure() {
  # Import our standard compiler defines
  standard_meson_build

  pushd "$THIS_SCRIPT_PATH" > /dev/null
  # Additional arguments are used by Linux build, e.g. -Dprefix=${INSTALLDIR}
  meson setup build --buildtype $BUILDER_CONFIGURATION "${builder_extra_params[@]}"
  popd > /dev/null

}

# ----------------------------------------------------------------------------
# build
# ----------------------------------------------------------------------------

do_meson_build() {
  pushd "$TARGET_PATH" > /dev/null
  ninja
  popd > /dev/null
}

# ----------------------------------------------------------------------------
# test
# ----------------------------------------------------------------------------

do_meson_test() {
  pushd "$TARGET_PATH" > /dev/null
  meson test "${builder_extra_params[@]}"
  popd > /dev/null
}