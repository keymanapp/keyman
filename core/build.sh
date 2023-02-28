#!/usr/bin/env bash

set -e
set -u

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/build-utils.sh"

## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$THIS_SCRIPT_PATH/commands.inc.sh"

cd "$THIS_SCRIPT_PATH"

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
  mac|linux)
    archtargets+=(
      ":arch   Linux or mac build -- current architecture"
    )
    ;;
esac

# TODO: consider using "linux" and "mac" instead of "arch"?
#  ":linux          Build for current Linux architecture"
#  ":mac            Build for current macOS architecture"

builder_describe \
"Build Keyman Core

Libraries will be built in 'build/<target>/<configuration>/src'.
  * <configuration>: 'debug' or 'release' (see --debug flag)
  * All parameters after '--' are passed to meson or ninja
" \
  "@/common/tools/hextobin" \
  "@/common/web/keyman-version" \
  "@/developer/src/kmc" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "install         install libraries to current system" \
  "uninstall       uninstall libraries from current system" \
  "${archtargets[@]}" \
  "--debug,-d                      configuration is 'debug', not 'release'" \
  "--no-tests      do not configure tests (used by other projects)" \
  "--target-path=opt_target_path   override for build/ target path" \
  "--test=opt_tests,-t             test[s] to run (space separated)"

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

if builder_has_option --debug; then
  CONFIGURATION=debug
else
  CONFIGURATION=release
fi

builder_describe_outputs \
  configure:x86      build/x86/$CONFIGURATION/build.ninja \
  configure:x64      build/x64/$CONFIGURATION/build.ninja \
  configure:arch     build/arch/$CONFIGURATION/build.ninja \
  configure:wasm     build/wasm/$CONFIGURATION/build.ninja \
  build:x86          build/x86/$CONFIGURATION/src/libkmnkbp0.a \
  build:x64          build/x64/$CONFIGURATION/src/libkmnkbp0.a \
  build:arch         build/arch/$CONFIGURATION/src/libkmnkbp0-static.a \
  build:wasm         build/wasm/$CONFIGURATION/src/libkmnkbp0.a

# Target path is used by Linux build, e.g. --target-path keyboardprocessor
if builder_has_option --target-path; then
  TARGET_PATH="$opt_target_path"
else
  TARGET_PATH="$KEYMAN_ROOT/core/build"
fi

# Iterate through all possible targets; note that targets that cannot be built
# on the current platform have already been excluded through the archtargets
# settings above
targets=(wasm x86 x64 arch)

for target in "${targets[@]}"; do
  MESON_PATH="$TARGET_PATH/$target/$CONFIGURATION"

  do_clean $target
  do_configure $target
  do_build $target
  do_test $target
  do_install $target
  do_uninstall $target
done
