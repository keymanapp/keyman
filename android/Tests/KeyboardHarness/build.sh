#!/usr/bin/env bash
#
# Tests: KeyboardHarness

set -eu
# set -x: Debugging use, print each statement
# set -x

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

################################ Main script ################################

builder_describe \
  "Build KeyboardHarness test app for Android." \
  clean \
  build \
  ":app                   KeyboardHarness" \
  "--ci                   Don't start the Gradle daemon. Use for CI" \
  "--debug,-d             Local debug build; use for development builds"

builder_parse "$@"

BUILD_FLAGS=""

# Build flags that apply to all targets
if builder_has_option --ci; then
  BUILD_FLAGS="$BUILD_FLAGS -no-daemon"
fi

if builder_has_option --debug; then
  BUILD_FLAGS="$BUILD_FLAGS assembleDebug"
else
  BUILD_FLAGS="$BUILD_FLAGS build"
fi

#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0

# Clean build artifacts: output and upload directories
function _clean() {
  cd "$KEYMAN_ROOT/android/Tests/KeyboardHarness"

  if [ -d "$KEYMAN_ROOT/android/Tests/KeyboardHarness/app/build/outputs" ]; then
    echo "Cleaning KeyboardHarness build outputs directory"
    rm -rf "$KEYMAN_ROOT/android/Tests/KeyboardHarness/app/build/outputs"
  fi

  if [ -d "$KEYMAN_ROOT/android/upload" ]; then
    echo "Cleaning upload directory"
    rm -rf "$KEYMAN_ROOT/android/upload"
  fi
}

function _build_app() {
  cd "$KEYMAN_ROOT/android/Tests/KeyboardHarness"
  ./gradlew clean $BUILD_FLAGS

  if [ $? -ne 0 ]; then
    die "ERROR: KeyboardHarness/build.sh failed"
  fi
}


# Check about cleaning artifact paths
if builder_start_action clean; then
  _clean
  builder_finish_action success clean
fi

# Building KeyboardHarness
if builder_start_action build:app; then
  _build_app
  builder_finish_action success build:app
fi
