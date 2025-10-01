#!/usr/bin/env bash
# Build FirstVoices for Android app

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/build-download-resources.sh"

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
  "test               Runs lint and tests." \
  "publish-symbols    Publishes symbols to Sentry." \
  "publish-play-store Publishes the APK to the Play Store."

# parse before describe_outputs to check debug flags
builder_parse "$@"

if builder_is_debug_build; then
  builder_heading "### Debug config ####"
  CONFIG="debug"
  BUILD_FLAGS="assembleDebug -x lintDebug -x testDebug"
  TEST_FLAGS="-x assembleDebug lintDebug testDebug"
fi

ARTIFACT="firstvoices-${KEYMAN_VERSION_FOR_FILENAME}.apk"

KEYBOARD_PACKAGE_ID="fv_all"
KEYBOARDS_TARGET="oem/firstvoices/android/app/src/main/assets/${KEYBOARD_PACKAGE_ID}.kmp"
builder_describe_outputs \
  configure     "/${KEYBOARDS_TARGET}" \
  build         /oem/firstvoices/android/app/build/outputs/apk/$CONFIG/${ARTIFACT}

#### Build

# Parse args

if builder_is_ci_build; then
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
  downloadKeyboardPackage "$KEYBOARD_PACKAGE_ID" "${KEYMAN_ROOT}/$KEYBOARDS_TARGET"

  builder_finish_action success configure
fi

if builder_start_action build; then
  # Copy Keyman Engine for Android
  cp "$KEYMAN_ROOT/android/KMEA/app/build/outputs/aar/keyman-engine-${CONFIG}.aar" "$KEYMAN_ROOT/oem/firstvoices/android/app/libs/keyman-engine.aar"

  echo "BUILD_FLAGS: $BUILD_FLAGS"
  ./gradlew $DAEMON_FLAG clean $BUILD_FLAGS

  mv "${KEYMAN_ROOT}/oem/firstvoices/android/app/build/outputs/apk/$CONFIG/firstvoices-${KEYMAN_VERSION}.apk" "${KEYMAN_ROOT}/oem/firstvoices/android/app/build/outputs/apk/$CONFIG/${ARTIFACT}"

  builder_finish_action success build
fi

if builder_start_action test; then
  echo "TEST_FLAGS: $TEST_FLAGS"
  ./gradlew $DAEMON_FLAG $TEST_FLAGS

  builder_finish_action success test
fi

publish_symbols() {
  if builder_is_ci_build && builder_is_ci_build_level_release; then
    # TODO: what does publishSentry even do?
    ./gradlew $DAEMON_FLAG publishSentry

    echo "Making a Sentry release for FV Keyboards for tag $KEYMAN_VERSION_GIT_TAG"
    sentry-cli upload-dif -p fv-android --include-sources
    sentry-cli releases -p fv-android files $KEYMAN_VERSION_GIT_TAG upload-sourcemaps ./
  fi
}

publish_play_store() {
  if builder_is_ci_build && builder_is_ci_build_level_release; then
    # Publish FV Keyboards to Play Store
    ./gradlew $DAEMON_FLAG publishReleaseApk
  fi
}

builder_run_action publish-symbols       publish_symbols
builder_run_action publish-play-store    publish_play_store

