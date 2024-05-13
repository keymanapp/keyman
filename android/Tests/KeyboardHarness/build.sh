#!/usr/bin/env bash
#
# Build Test app: KeyboardHarness
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

################################ Main script ################################

# Definition of global compile constants
CONFIG="release"
BUILD_FLAGS="aR -x lint -x test"           # Gradle build w/o test
TEST_FLAGS="-x aR lintRelease testRelease" # Gradle test w/o build

builder_describe "Build KeyboardHarness test app for Android." \
  "@/android/KMEA" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  ":app                   KeyboardHarness" \
  "--ci                   Don't start the Gradle daemon. Use for CI"

# parse before describe outputs to check debug flags
builder_parse "$@"

ARTIFACT="app-release-unsigned.apk"

if builder_is_debug_build; then
  builder_heading "### Debug config ####"
  CONFIG="debug"
  BUILD_FLAGS="assembleDebug -x lint -x test"
  TEST_FLAGS="-x assembleDebug lintDebug testDebug"
  ARTIFACT="app-$CONFIG.apk"
fi


builder_describe_outputs \
  configure    /android/Tests/KeyboardHarness/app/libs/keyman-engine.aar \
  build:app    /android/Tests/KeyboardHarness/app/build/outputs/apk/$CONFIG/${ARTIFACT}

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
if builder_start_action clean:app; then
  rm -rf "$KEYMAN_ROOT/android/Tests/KeyboardHarness/app/build/outputs"
  builder_finish_action success clean:app
fi

if builder_start_action configure:app; then

  builder_finish_action success configure:app
fi

# Building KeyboardHarness
if builder_start_action build:app; then

  # Copy Keyman Engine for Android
  cp "$KEYMAN_ROOT/android/KMEA/app/build/outputs/aar/keyman-engine-${CONFIG}.aar" "$KEYMAN_ROOT/android/Tests/KeyboardHarness/app/libs/keyman-engine.aar"

  echo "BUILD_FLAGS: $BUILD_FLAGS"
  ./gradlew clean $BUILD_FLAGS
  builder_finish_action success build:app
fi

if builder_start_action test:app; then
  echo "TEST_FLAGS $TEST_FLAGS"
  # TODO: define tests
  builder_finish_action success test:app
fi
