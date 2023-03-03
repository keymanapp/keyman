#!/usr/bin/env bash
#
# Build Test app: KeyboardHarness

#set -x
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

################################ Main script ################################

# Definition of global compile constants
CONFIG="release"
BUILD_FLAGS="aR -x lint -x test"           # Gradle build w/o test
TEST_FLAGS="-x aR lintRelease testRelease" # Gradle test w/o build

builder_describe "Build KeyboardHarness test app for Android." \
  "@../../KMEA" \
  "clean" \
  "configure" \
  "build" \
  ":app                   KeyboardHarness" \
  "--ci                   Don't start the Gradle daemon. Use for CI" 

# parse before describe outputs to check debug flags  
builder_parse "$@"

if builder_has_option --debug; then
  builder_heading "### Debug config ####"
  CONFIG="debug"
  BUILD_FLAGS="assembleDebug -x lint -x test"
  TEST_FLAGS="-x assembleDebug lintDebug testDebug"
fi

ARTIFACT="app-$CONFIG.apk"

builder_describe_outputs \
  configure    app/libs/keyman-engine.aar \
  build:app    app/build/outputs/apk/$CONFIG/${ARTIFACT}

#### Build


#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0


# Parse args

# Build flags that apply to all targets
if builder_has_option --ci; then
  BUILD_FLAGS="$BUILD_FLAGS -no-daemon"
  TEST_FLAGS="$TEST_FLAGS -no-daemon"
fi

#### Build action definitions ####

# Check about cleaning artifact paths
if builder_start_action clean; then
  rm -rf "$KEYMAN_ROOT/android/Tests/KeyboardHarness/app/build/outputs"
  builder_finish_action success clean
fi

if builder_start_action configure; then
  # Copy Keyman Engine for Android
  cp "$KEYMAN_ROOT/android/KMEA/app/build/outputs/aar/${CONFIG}/keyman-engine.aar" "$KEYMAN_ROOT/android/Tests/KeyboardHarness/app/libs/keyman-engine.aar"

  builder_finish_action success configure
fi

# Building KeyboardHarness
if builder_start_action build:app; then

  echo "BUILD_FLAGS: $BUILD_FLAGS"
  ./gradlew clean $BUILD_FLAGS
  builder_finish_action success build:app
fi

if builder_start_action test; then
  echo "TEST_FLAGS $TEST_FLAGS"

  builder_finish_action succes test
fi
