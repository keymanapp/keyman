#!/usr/bin/env bash

set -e
set -u

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$THIS_SCRIPT_PATH/commands.inc.sh"

cd "$THIS_SCRIPT_PATH"

################################ Main script ################################

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
  "Build Keyman KMX Compiler Static Library
Libraries will be built in 'build/<target>/<configuration>/src'.
  * <configuration>: 'debug' or 'release' (see --debug flag)
  * All parameters after '--' are passed to meson or ninja
" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "${archtargets[@]}" \
  "--debug,-d                      configuration is 'debug', not 'release'" \
  "--test=opt_tests,-t             test[s] to run (space separated)"

builder_parse "$@"

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
  build:x86          build/x86/$CONFIGURATION/src/libkmcmplib.a \
  build:x64          build/x64/$CONFIGURATION/src/libkmcmplib.a \
  build:arch         build/arch/$CONFIGURATION/src/libkmcmplib.a \
  build:wasm         build/wasm/$CONFIGURATION/src/libkmcmplib.a

TARGET_PATH="$THIS_SCRIPT_PATH/build"

# Iterate through all possible targets; note that targets that cannot be built
# on the current platform have already been excluded through the archtargets
# settings above
targets=(wasm x86 x64 arch)

do_action() {
  local action_function=do_$1
  for target in "${targets[@]}"; do
    MESON_PATH="$TARGET_PATH/$target/$CONFIGURATION"
    $action_function $target
  done
}

do_action clean

if builder_has_action configure; then
  # Import our standard compiler defines; this is copied from
  # /resources/build/meson/standard.meson.build by build.sh, because meson doesn't
  # allow us to reference a file outside its root
  mkdir -p "$THIS_SCRIPT_PATH/resources"
  cp "$KEYMAN_ROOT/resources/build/meson/standard.meson.build" "$THIS_SCRIPT_PATH/resources/meson.build"
fi

do_action configure
do_action build
do_action test
