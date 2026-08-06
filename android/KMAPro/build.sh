#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/build-help.inc.sh"
. "$KEYMAN_ROOT/resources/build/build-download-resources.sh"
. "$KEYMAN_ROOT/android/KMAPro/build-play-store-notes.inc.sh"

# ################################ Main script ################################

builder_describe "Builds Keyman for Android app." \
  "@/android/KMEA" \
  "clean" \
  "configure" \
  "build" \
  "test                Runs lint and unit tests." \
  "publish-symbols     Publishes symbols to Sentry." \
  "publish-play-store  Publishes the APK to the Play Store."

builder_parse "$@"

if builder_is_debug_build; then
  BUILD_FLAGS="assembleDebug -x lintDebug -x testDebug"
  TEST_FLAGS="-x assembleDebug lintDebug testDebug"
else
  BUILD_FLAGS="build -x lint -x test"                     # Gradle build w/o test
  TEST_FLAGS="-x assembleRelease lintRelease testRelease" # Gradle test w/o build
fi

if builder_is_ci_build; then
  GRADLE_DAEMON=--no-daemon
else
  GRADLE_DAEMON=
fi

# Locations for bundled keyboard and lexical model

KEYBOARD_PACKAGE_ID="sil_euro_latin"
KEYBOARDS_TARGET="android/KMAPro/kMAPro/src/main/assets/${KEYBOARD_PACKAGE_ID}.kmp"
MODEL_PACKAGE_ID="nrc.en.mtnt"
MODELS_TARGET="android/KMAPro/kMAPro/src/main/assets/${MODEL_PACKAGE_ID}.model.kmp"

builder_describe_outputs \
  configure     "/${MODELS_TARGET}" \
  build         /android/KMAPro/kMAPro/build/outputs/apk/$BUILDER_CONFIGURATION/keyman-${KEYMAN_VERSION_FOR_FILENAME}.apk

#### Build action definitions ####

do_configure() {
  downloadKeyboardPackage "$KEYBOARD_PACKAGE_ID" "${KEYMAN_ROOT}/$KEYBOARDS_TARGET"
  downloadModelPackage "$MODEL_PACKAGE_ID" "${KEYMAN_ROOT}/$MODELS_TARGET"
}

do_build() {
  # Copy Keyman Engine for Android
  cp "${KEYMAN_ROOT}/android/KMEA/app/build/outputs/aar/keyman-engine-${BUILDER_CONFIGURATION}.aar" "${KEYMAN_ROOT}/android/KMAPro/kMAPro/libs/keyman-engine.aar"

  # Convert markdown to html for offline help
  build_help_html android KMAPro/kMAPro/src/main/assets/info

  builder_echo "BUILD_FLAGS $BUILD_FLAGS"
  ./gradlew $GRADLE_DAEMON $BUILD_FLAGS

  mv "${KEYMAN_ROOT}/android/KMAPro/kMAPro/build/outputs/apk/$BUILDER_CONFIGURATION/keyman-${KEYMAN_VERSION}.apk" "${KEYMAN_ROOT}/android/KMAPro/kMAPro/build/outputs/apk/$BUILDER_CONFIGURATION/keyman-${KEYMAN_VERSION_FOR_FILENAME}.apk"
}

do_test() {
  builder_echo "TEST_FLAGS $TEST_FLAGS"
  ./gradlew $GRADLE_DAEMON $TEST_FLAGS
}

do_publish_symbols() {
  if builder_is_ci_build && builder_is_ci_build_level_release; then
    # TODO: what does publishSentry even do?
    ./gradlew $GRADLE_DAEMON publishSentry
    builder_echo "Making a Sentry release for tag $KEYMAN_VERSION_GIT_TAG"
    sentry-cli upload-dif -p keyman-android --include-sources
    sentry-cli releases -p keyman-android files $KEYMAN_VERSION_GIT_TAG upload-sourcemaps ./
  fi
}

do_publish_play_store() {
  if builder_is_ci_build && builder_is_ci_build_level_release; then
    generateReleaseNotes
    # Publish Keyman for Android to Play Store
    ./gradlew $GRADLE_DAEMON publishReleaseApk
  fi
}

builder_run_action clean               rm -rf "$KEYMAN_ROOT/android/KMAPro/kMAPro/build"
builder_run_action configure           do_configure
builder_run_action build               do_build
builder_run_action test                do_test
builder_run_action publish-symbols     do_publish_symbols
builder_run_action publish-play-store  do_publish_play_store
