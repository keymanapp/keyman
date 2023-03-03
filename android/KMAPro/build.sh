#!/usr/bin/env bash
# Build Keyman for Android app (KMAPro)

# set -x
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
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

builder_describe "Builds Keyman for Android app." \
  "@../KMEA" \
  "clean" \
  "configure" \
  "build" \
  "test             Runs lint and unit tests." \
  "--ci             Don't start the Gradle daemon. For CI" \
  "--upload-sentry  Upload to sentry"

# parse before describe_outputs to check debug flags
builder_parse "$@"

if builder_has_option --debug; then
  builder_heading "### Debug config ####"
  CONFIG="debug"
  BUILD_FLAGS="assembleDebug -x lint -x test"
  TEST_FLAGS="-x assembleDebug lintDebug testDebug"
fi

builder_describe_outputs \
  configure     kMAPro/libs/keyman-engine.aar \
  build         kMAPro/build/outputs/apk/$CONFIG/keyman-${VERSION}.apk

#### Build

function convert_markdown_to_html() {
  echo "Converting markdown to html for offline help"
  pushd "$KEYMAN_ROOT/android"
  ./build-help.sh htm
  popd
}



# Parse args

if builder_has_option --ci; then
  DAEMON_FLAG=--no-daemon
fi

#### Build action definitions ####

function makeLocalSentryRelease() {
  echo "Making a Sentry release for tag $VERSION_GIT_TAG"
  sentry-cli upload-dif -p keyman-android --include-sources
  sentry-cli releases -p keyman-android files $VERSION_GIT_TAG upload-sourcemaps ./

  echo "Finalizing release tag $VERSION_GIT_TAG"
  sentry-cli releases finalize "$VERSION_GIT_TAG"
}


#### Build action definitions ####

# Check about cleaning artifact paths and upload directories
if builder_start_action clean; then

  rm -rf "$KEYMAN_ROOT/android/KMAPro/kMAPro/build/outputs"

  rm -rf "$KEYMAN_ROOT/android/upload"

  builder_finish_action success clean
fi

if builder_start_action configure; then

  # Copy Keyman Engine for Android
  cp "$KEYMAN_ROOT/android/KMEA/app/build/outputs/aar/${CONFIG}/keyman-engine.aar" "$KEYMAN_ROOT/android/KMAPro/kMAPro/libs/keyman-engine.aar"

  KEYBOARD_PACKAGE_ID="sil_euro_latin"
  KEYBOARDS_TARGET="$KEYMAN_ROOT/android/KMAPro/kMAPro/src/main/assets/${KEYBOARD_PACKAGE_ID}.kmp"
  MODEL_PACKAGE_ID="nrc.en.mtnt"
  MODELS_TARGET="$KEYMAN_ROOT/android/KMAPro/kMAPro/src/main/assets/${MODEL_PACKAGE_ID}.model.kmp"

  downloadKeyboardPackage "$KEYBOARD_PACKAGE_ID" "$KEYBOARDS_TARGET"
  downloadModelPackage "$MODEL_PACKAGE_ID" "$MODELS_TARGET"

  builder_finish_action success configure
fi

if builder_start_action build; then

  # Convert markdown to html for offline help
  convert_markdown_to_html

  echo "BUILD_FLAGS $BUILD_FLAGS"
  ./gradlew $DAEMON_FLAG clean $BUILD_FLAGS

  if builder_has_option --upload-sentry; then
    makeLocalSentryRelease
  fi

  builder_finish_action success build
fi

if builder_start_action test; then

  echo "TEST_FLAGS $TEST_FLAGS"
  ./gradlew $DAEMON_FLAG $TEST_FLAGS

  builder_finish_action success test
fi
