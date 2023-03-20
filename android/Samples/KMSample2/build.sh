#!/usr/bin/env bash
#
# Samples: KMSample2

set -eu
# set -x: Debugging use, print each statement
# set -x

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
SAMPLE_FLAGS="build"

builder_describe "Build KMSample2 app for Android." \
  "@/android/KMEA" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  ":app                   KMSample2" \
  "--ci                   Don't start the Gradle daemon. Use for CI"

# parse before describe_outputs to check debug flags
builder_parse "$@"

if builder_is_debug_build; then
  builder_heading "### Debug config ####"
  CONFIG="debug"
  SAMPLE_FLAGS="assembleDebug"
fi

ARTIFACT="app-$CONFIG.apk"


builder_describe_outputs \
  configure             /android/Samples/KMSample2/app/libs/keyman-engine.aar \
  build:app             /android/Samples/KMSample2/app/build/outputs/apk/$CONFIG/$ARTIFACT



# Parse args

if builder_has_option --ci; then
  SAMPLE_FLAGS="$SAMPLE_FLAGS -no-daemon"
fi

#### Build action definitions ####

# Check about cleaning artifact paths
if builder_start_action clean:app; then
  rm -rf "$KEYMAN_ROOT/android/Samples/KMSample2/app/build/outputs"
  builder_finish_action success clean:app
fi

if builder_start_action configure:app; then
  # Copy Keyman Engine for Android
  cp "$KEYMAN_ROOT/android/KMEA/app/build/outputs/aar/${CONFIG}/keyman-engine.aar" "$KEYMAN_ROOT/android/Samples/KMSample2/app/libs/keyman-engine.aar"

  builder_finish_action success configure:app
fi

# Building KMSample2
if builder_start_action build:app; then
  ./gradlew clean $SAMPLE_FLAGS

  builder_finish_action success build:app
fi

if builder_start_action test:app; then
  # TODO: define tests
  builder_finish_action success test:app
fi
