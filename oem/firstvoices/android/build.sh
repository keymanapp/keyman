#!/usr/bin/env bash
# Build FirstVoices for Android app

# set -x
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

. "$KEYMAN_ROOT/resources/build/build-download-resources.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

# ################################ Main script ################################

# Definition of global compile constants

CONFIG="release"
BUILD_FLAGS="build -x lint -x test"                     # Gradle build w/o test
TEST_FLAGS="-x assembleRelease lintRelease testRelease" # Gradle test w/o build
DAEMON_FLAG=

builder_describe "Builds FirstVoices for Android app." \
  "@/android/KMEA" \
  "clean" \
  "configure" \
  "build" \
  "test             Runs lint and tests." \
  "publish          Publishes symbols to Sentry and the APK to the Play Store." \
  "--ci             Don't start the Gradle daemon. For CI" \
  "--upload-sentry  Upload to sentry"

# parse before describe_outputs to check debug flags
builder_parse "$@"

if builder_is_debug_build; then
  builder_heading "### Debug config ####"
  CONFIG="debug"
  BUILD_FLAGS="assembleDebug -x lint -x test"
  TEST_FLAGS="-x assembleDebug lintDebug testDebug"
fi  

ARTIFACT="firstvoices-$VERSION.apk"

builder_describe_outputs \
  configure     /oem/firstvoices/android/app/libs/keyman-engine.aar \
  build         /oem/firstvoices/android/app/build/outputs/apk/$CONFIG/${ARTIFACT}

#### Build

# Parse args

if builder_has_option --ci; then
  DAEMON_FLAG=--no-daemon
fi

#### Build action definitions ####

# Check about cleaning artifact paths
if builder_start_action clean; then
  rm -rf "$KEYMAN_ROOT/oem/firstvoices/android/app/build/outputs"
  rm -rf "$KEYMAN_ROOT/oem/firstvoices/android/app/build/tmp"
  builder_finish_action success clean
fi

if builder_start_action configure; then
  KEYBOARDS_CSV="$KEYMAN_ROOT/oem/firstvoices/keyboards.csv"
  KEYBOARDS_CSV_TARGET="$KEYMAN_ROOT/oem/firstvoices/android/app/src/main/assets/keyboards.csv"

  KEYBOARD_PACKAGE_ID="fv_all"
  KEYBOARDS_TARGET="$KEYMAN_ROOT/oem/firstvoices/android/app/src/main/assets/${KEYBOARD_PACKAGE_ID}.kmp"

  cp "$KEYBOARDS_CSV" "$KEYBOARDS_CSV_TARGET"
  downloadKeyboardPackage "$KEYBOARD_PACKAGE_ID" "$KEYBOARDS_TARGET"

  builder_finish_action success configure
fi

if builder_start_action build; then
  # Copy Keyman Engine for Android
  cp "$KEYMAN_ROOT/android/KMEA/app/build/outputs/aar/keyman-engine-${CONFIG}.aar" "$KEYMAN_ROOT/oem/firstvoices/android/app/libs/keyman-engine.aar"

  echo "BUILD_FLAGS: $BUILD_FLAGS"
  ./gradlew $DAEMON_FLAG clean $BUILD_FLAGS

  builder_finish_action success build
fi

if builder_start_action test; then
  echo "TEST_FLAGS: $TEST_FLAGS"
  ./gradlew $DAEMON_FLAG $TEST_FLAGS

  builder_finish_action success test
fi

builder_run_action publish    ./gradlew $DAEMON_FLAG publishSentry publishReleaseApk
