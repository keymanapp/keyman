#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$THIS_SCRIPT_PATH/checkout-keyboards.inc.sh"
. "$THIS_SCRIPT_PATH/commands.inc.sh"

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
  "@/common/include" \
  "clean" \
  "configure" \
  "build" \
  "api" \
  "test" \
  "${archtargets[@]}" \
  "--full-test                     also run expensive tests that rely on keyboards repo" \
  "--test=opt_tests,-t             test[s] to run (space separated)"

builder_parse "$@"

builder_describe_outputs \
  configure:x86      build/x86/$BUILDER_CONFIGURATION/build.ninja \
  configure:x64      build/x64/$BUILDER_CONFIGURATION/build.ninja \
  configure:arch     build/arch/$BUILDER_CONFIGURATION/build.ninja \
  configure:wasm     build/wasm/$BUILDER_CONFIGURATION/build.ninja \
  build:x86          build/x86/$BUILDER_CONFIGURATION/src/libkmcmplib.a \
  build:x64          build/x64/$BUILDER_CONFIGURATION/src/libkmcmplib.a \
  build:arch         build/arch/$BUILDER_CONFIGURATION/src/libkmcmplib.a \
  build:wasm         build/wasm/$BUILDER_CONFIGURATION/src/libkmcmplib.a

TARGET_PATH="$THIS_SCRIPT_PATH/build"

# Iterate through all possible targets; note that targets that cannot be built
# on the current platform have already been excluded through the archtargets
# settings above
targets=(wasm x86 x64 arch)

do_action() {
  local action_function=do_$1
  for target in "${targets[@]}"; do
    MESON_PATH="$TARGET_PATH/$target/$BUILDER_CONFIGURATION"
    $action_function $target
  done
}

if builder_has_option --full-test; then
  locate_keyboards_repo
fi

# Note, we have a 'global' clean and also a per-arch clean
if builder_start_action clean; then
  rm -rf "$THIS_SCRIPT_PATH/tests/keyboards"
  builder_finish_action success clean
fi

do_action clean

# Note, we have a 'global' configure and also a per-arch configure
if builder_start_action configure; then
  # Import our standard compiler defines; this is copied from
  # /resources/build/meson/standard.meson.build by build.sh, because meson doesn't
  # allow us to reference a file outside its root
  mkdir -p "$THIS_SCRIPT_PATH/resources"
  cp "$KEYMAN_ROOT/resources/build/meson/standard.meson.build" "$THIS_SCRIPT_PATH/resources/meson.build"

  # We have to checkout the keyboards repo in a 'configure' action because
  # otherwise meson will not get the right list of keyboard source files,
  # even though we only use it in the 'test' action
  if builder_has_option --full-test; then
    checkout_keyboards
  fi

  builder_finish_action success configure
fi

do_action configure
do_action build
do_action test