#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/build-utils.sh"

## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$THIS_SCRIPT_PATH/commands.inc.sh"

################################ Main script ################################

cleanup_visual_studio_path

MESON_LOW_VERSION=false

if [[ `meson --version` < 0.54 ]]; then
  MESON_LOW_VERSION=true
fi

#
# Restrict available targets to those that can be built on the current system
#

archtargets=(":wasm   WASM build")

case $BUILDER_OS in
  win)
    archtargets+=(
      ":x86    32-bit Windows (x86) build"
      ":x64    64-bit Windows (x64) build"
    )
    ;;
  mac)
    archtargets+=(
      ":mac           Mac all architectures fat library build"
      ":mac-x86_64    Mac Intel build"
      ":mac-arm64     Mac arm64 (M1) build"
    )
    ;;
  linux)
    archtargets+=(
      ":arch   Linux build -- current architecture"
    )
    ;;
esac

# TODO: consider using "linux" instead of "arch"?
#  ":linux          Build for current Linux architecture"

builder_describe \
"Build Keyman Core

Libraries will be built in 'build/<target>/<configuration>/src'.
  * <configuration>: 'debug' or 'release' (see --debug flag)
  * All parameters after '--' are passed to meson or ninja \
" \
  "@/common/tools/hextobin" \
  "@/common/web/keyman-version" \
  "@/developer/src/kmc" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "install                         install libraries to current system" \
  "uninstall                       uninstall libraries from current system" \
  "${archtargets[@]}" \
  "--no-tests                      do not configure tests (used by other projects)" \
  "--test,-t=opt_tests             test[s] to run (space separated)"

builder_parse "$@"

#
# meson forces us to configure tests, including building compilers, even
# if we don't plan to run them, for example when doing a dependency build
# in CI
#
MESON_OPTION_keyman_core_tests=
BUILD_BAT_keyman_core_tests=

if builder_is_dep_build || builder_has_option --no-tests; then
  MESON_OPTION_keyman_core_tests="-Dkeyman_core_tests=false"
  BUILD_BAT_keyman_core_tests=--no-tests
  builder_remove_dep /common/tools/hextobin
  builder_remove_dep /common/web/keyman-version
  builder_remove_dep /developer/src/kmc
fi

# 'mac' target builds both x86_64 and arm architectures and
# generates a 'fat' library from them.
builder_describe_internal_dependency \
  build:mac build:mac-x86_64 \
  build:mac build:mac-arm64

builder_describe_outputs \
  configure:x86             /core/build/x86/$BUILDER_CONFIGURATION/build.ninja \
  configure:x64             /core/build/x64/$BUILDER_CONFIGURATION/build.ninja \
  configure:mac             /core/build/mac/$BUILDER_CONFIGURATION/ \
  configure:mac-x86_64      /core/build/mac-x86_64/$BUILDER_CONFIGURATION/build.ninja \
  configure:mac-arm64       /core/build/mac-arm64/$BUILDER_CONFIGURATION/build.ninja \
  configure:arch            /core/build/arch/$BUILDER_CONFIGURATION/build.ninja \
  configure:wasm            /core/build/wasm/$BUILDER_CONFIGURATION/build.ninja \
  build:x86                 /core/build/x86/$BUILDER_CONFIGURATION/src/libkeymancore.a \
  build:x64                 /core/build/x64/$BUILDER_CONFIGURATION/src/libkeymancore.a \
  build:mac                 /core/build/mac/$BUILDER_CONFIGURATION/libkeymancore.a \
  build:mac-x86_64          /core/build/mac-x86_64/$BUILDER_CONFIGURATION/src/libkeymancore.a \
  build:mac-arm64           /core/build/mac-arm64/$BUILDER_CONFIGURATION/src/libkeymancore.a \
  build:arch                /core/build/arch/$BUILDER_CONFIGURATION/src/libkeymancore.a \
  build:wasm                /core/build/wasm/$BUILDER_CONFIGURATION/src/libkeymancore.a

# Import our standard compiler defines; this is copied from
# /resources/build/meson/standard.meson.build by build.sh, because meson doesn't
# allow us to reference a file outside its root
if builder_has_action configure; then
  mkdir -p "$THIS_SCRIPT_PATH/resources"
  cp "$KEYMAN_ROOT/resources/build/meson/standard.meson.build" "$THIS_SCRIPT_PATH/resources/meson.build"
fi

# Iterate through all possible targets; note that targets that cannot be built
# on the current platform have already been excluded through the archtargets
# settings above
targets=(wasm x86 x64 mac-x86_64 mac-arm64 arch)

do_action() {
  local action_function=do_$1
  for target in "${targets[@]}"; do
    MESON_PATH="$KEYMAN_ROOT/core/build/$target/$BUILDER_CONFIGURATION"
    $action_function $target
  done
}

# -------------------------------------------------------------------------------

do_action clean

# -------------------------------------------------------------------------------

do_action configure

# After we have built the necessary internal dependencies, then we can go
# ahead and build a fat library for external consumption
if builder_start_action configure:mac; then
  mkdir -p "$KEYMAN_ROOT/core/build/mac/$BUILDER_CONFIGURATION"
  builder_finish_action success configure:mac
fi

# -------------------------------------------------------------------------------

do_action build

if builder_start_action build:mac; then
  lipo -create \
    "$KEYMAN_ROOT/core/build/mac-x86_64/$BUILDER_CONFIGURATION/src/libkeymancore.a" \
    "$KEYMAN_ROOT/core/build/mac-arm64/$BUILDER_CONFIGURATION/src/libkeymancore.a" \
    -output "$KEYMAN_ROOT/core/build/mac/$BUILDER_CONFIGURATION/libkeymancore.a"
  builder_finish_action success build:mac
fi

# -------------------------------------------------------------------------------

testparams="${builder_extra_params[@]} --print-errorlogs"
if builder_has_option --test; then
  testparams="$opt_tests $testparams"
fi

do_action test

if builder_start_action test:mac; then
  # We can only run the tests for the current architecture; we can
  # assume that build:mac has run so both architectures will be
  # available
  target=mac-`uname -m`
  MESON_PATH="$KEYMAN_ROOT/core/build/$target/$BUILDER_CONFIGURATION"
  meson test -C "$MESON_PATH" $testparams
  builder_finish_action success test:mac
fi

# -------------------------------------------------------------------------------

do_action install
do_action uninstall
