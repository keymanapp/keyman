#!/usr/bin/env bash
#
# Build Keyman Engine for Android, Keyman for Android, OEM FirstVoices Android app,
# Samples: KMsample1 and KMSample2, Test - KeyboardHarness

#set -x
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

################################ Main script ################################

builder_describe \
  "Build Keyman Engine for Android, Keyman for Android, and FirstVoices Android app." \
  clean \
  build \
  "publish                                  Publishes the APKs to the Play Store." \
  ":engine=KMEA                             Keyman Engine for Android" \
  ":app=KMAPro                              Keyman for Android" \
  ":sample1=Samples/KMSample1               Sample apps: KMSample1" \
  ":sample2=Samples/KMSample2               Sample app:  KMSample2" \
  ":keyboardharness=Tests/KeyboardHarness   Test app: KeyboardHarness" \
  ":fv=../oem/firstvoices/android           OEM FirstVoices for Android app" \
  "--ci                                     Don't start the Gradle daemon. Use for CI" \
  "--upload-sentry                          Uploads debug symbols, etc, to Sentry" 

builder_parse "$@"

DEBUG_FLAG=""
CI_FLAG=""
SENTRY_FLAG=""

if builder_has_option --ci; then
  CI_FLAG="--ci"
fi

if builder_has_option --debug; then
  DEBUG_FLAG="--debug"
fi

if builder_has_option --upload-sentry; then
  SENTRY_FLAG="--upload-sentry"
fi

#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0

# Clean build artifacts: keyman-engine.aar libaries, output and upload directories
function _clean() {
  cd "$KEYMAN_ROOT/android/KMEA"
  ./build.sh clean

  cd "$KEYMAN_ROOT/android/KMAPro"
  ./build.sh clean

  cd "$KEYMAN_ROOT/android/Samples/KMSample1"
  ./build.sh clean

  cd "$KEYMAN_ROOT/android/Samples/KMSample2"
  ./build.sh clean

  cd "$KEYMAN_ROOT/android/Tests/KeyboardHarness"
  ./build.sh clean

  cd "$KEYMAN_ROOT/oem/firstvoices/android"
  ./build.sh clean
}

# Check about cleaning artifact paths
if builder_start_action clean; then
  _clean
  builder_finish_action success clean
fi

# Building Keyman Engine for Android
if builder_start_action build:engine; then
  cd "$KEYMAN_ROOT/android/KMEA"
  ./build.sh build:engine $CI_FLAG $DEBUG_FLAG
  builder_finish_action success build:engine
fi

# Building Keyman for Android
if builder_start_action build:app; then
  cd "$KEYMAN_ROOT/android/KMAPro"
  ./build.sh build $CI_FLAG $DEBUG_FLAG $SENTRY_FLAG
  builder_finish_action success build:app
fi

# Building KMSample1 app
if builder_start_action build:sample1; then
  cd "$KEYMAN_ROOT/android/Samples/KMSample1"
  ./build.sh build $CI_FLAG $DEBUG_FLAG
  builder_finish_action success build:sample1
fi

# Building KMSample2 app
if builder_start_action build:sample2; then
  cd "$KEYMAN_ROOT/android/Samples/KMSample2"
  ./build.sh build $CI_FLAG $DEBUG_FLAG
  builder_finish_action success build:sample2
fi

# Building KeyboardHarness app
if builder_start_action build:keyboardharness; then
  cd "$KEYMAN_ROOT/android/Tests/KeyboardHarness"
  ./build.sh build $CI_FLAG $DEBUG_FLAG
  builder_finish_action success build:keyboardharness
fi

# Building OEM app
if builder_start_action build:fv; then
  cd "$KEYMAN_ROOT/oem/firstvoices/android"
  ./build.sh build $CI_FLAG $DEBUG_FLAG
  builder_finish_action success build:fv
fi

# Publish Keyman for Android to Play Store
if builder_start_action publish:app; then
  echo "publishing Keyman for Android"

  cd "$KEYMAN_ROOT/android"
  ./build-publish.sh -no-daemon -kmapro
  builder_finish_action success publish:app
fi

if builder_start_action publish:fv; then
  echo "publishing OEM FirstVoices app"

  cd "$KEYMAN_ROOT/android"
  ./build-publish.sh -no-daemon -fv
  builder_finish_action success publish:fv
fi
