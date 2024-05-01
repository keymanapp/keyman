#!/usr/bin/env bash
#
# Samples: KMsample1
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

################################ Main script ################################

# Definition of global compile constants

CONFIG="release"
SAMPLE_FLAGS="build"

builder_describe "Build KMSample1 app for Android." \
  "@/android/KMEA" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  ":app                   KMSample1" \
  "--ci                   Don't start the Gradle daemon. Use for CI"

# parse before describe_outputs to check debug flags
builder_parse "$@"

ARTIFACT="app-release-unsigned.apk"

if builder_is_debug_build; then
  builder_heading "### Debug config ####"
  CONFIG="debug"
  SAMPLE_FLAGS="assembleDebug"
  ARTIFACT="app-$CONFIG.apk"
fi


builder_describe_outputs \
  configure             /android/Samples/KMSample1/app/libs/keyman-engine.aar \
  build:app             /android/Samples/KMSample1/app/build/outputs/apk/$CONFIG/$ARTIFACT



# Parse args

if builder_has_option --ci; then
  SAMPLE_FLAGS="$SAMPLE_FLAGS -no-daemon"
fi

#### Build action definitions ####

# Check about cleaning artifact paths
if builder_start_action clean:app; then
  rm -rf "$KEYMAN_ROOT/android/Samples/KMSample1/app/build/outputs"
  builder_finish_action success clean:app
fi

if builder_start_action configure:app; then

  builder_finish_action success configure:app
fi

# Building KMSample1
if builder_start_action build:app; then

  # Copy Keyman Engine for Android
  cp "$KEYMAN_ROOT/android/KMEA/app/build/outputs/aar/keyman-engine-${CONFIG}.aar" "$KEYMAN_ROOT/android/Samples/KMSample1/app/libs/keyman-engine.aar"

  ./gradlew clean $SAMPLE_FLAGS

  builder_finish_action success build:app
fi

if builder_start_action test:app; then
  # TODO: define tests
  builder_finish_action success test:app
fi
