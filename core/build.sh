#!/usr/bin/env bash

set -e
set -u

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$THIS_SCRIPT_PATH/commands.inc.sh"

cd "$THIS_SCRIPT_PATH"

################################ Main script ################################

get_builder_OS

#
# Restrict available targets to those that can be built on the current system
#

archtargets=(":wasm   WASM build")

case $os_id in
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
  "@/common/tools/hextobin      test" \
  "@/common/web/keyman-version  test" \
  "@/developer/src/kmc          test" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "install         install libraries to current system" \
  "uninstall       uninstall libraries from current system" \
  "${archtargets[@]}" \
  "--debug,-d                      configuration is 'debug', not 'release'" \
  "--target-path=opt_target_path   override for build/ target path" \
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
  build:x86          build/x86/$CONFIGURATION/src/libkmnkbp0.a \
  build:x64          build/x64/$CONFIGURATION/src/libkmnkbp0.a \
  build:arch         build/arch/$CONFIGURATION/src/libkmnkbp0.a \
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
