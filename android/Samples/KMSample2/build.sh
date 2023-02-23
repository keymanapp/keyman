#!/usr/bin/env bash
#
# Samples: KMSample2

set -eu
# set -x: Debugging use, print each statement
# set -x

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

################################ Main script ################################

builder_describe "Build KMSample2 app for Android." \
  "@../../KMEA configure" \
  "clean" \
  "build" \
  ":app                   KMSample1" \
  "--ci                   Don't start the Gradle daemon. Use for CI" 

# parse before describe_outputs to check debug flags
builder_parse "$@"

CONFIG="release"
SAMPLE_FLAGS="build"
ARTIFACT="app-release.apk"

if builder_has_option --debug; then
  CONFIG="debug"
  SAMPLE_FLAGS="assembleDebug"
  ARTIFACT="app-debug.apk"
fi

# Build flags that apply to all targets
if builder_has_option --ci; then
  SAMPLE_FLAGS="$SAMPLE_FLAGS -no-daemon"
fi

builder_describe_outputs \
  build:app             ./app/build/outputs/apk/$CONFIG/$ARTIFACT

#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0

# Check about cleaning artifact paths and upload directories
if builder_start_action clean; then
  cd "$KEYMAN_ROOT/android/Samples/KMSample2/"

  if [ -d "$KEYMAN_ROOT/android/Samples/KMSample2/app/build/outputs" ]; then
    echo "Cleaning KMSample2 build outputs directory"
    rm -rf "$KEYMAN_ROOT/android/Samples/KMSample2/app/build/outputs"
  fi

  if [ -d "$KEYMAN_ROOT/android/upload" ]; then
    echo "Cleaning upload directory"
    rm -rf "$KEYMAN_ROOT/android/upload"
  fi

  builder_finish_action success clean
fi

# Building KMSample2
if builder_start_action build:app; then
  cd "$KEYMAN_ROOT/android/Samples/KMSample2"
  ./gradlew clean $SAMPLE_FLAGS

  if [ $? -ne 0 ]; then
    builder_die "ERROR: KMSample2/build.sh failed"
  fi

  builder_finish_action success build:app
fi
