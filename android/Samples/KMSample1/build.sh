#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "${KEYMAN_ROOT}/android/android-utils.inc.sh"

################################ Main script ################################

builder_describe "Build KMSample1 app for Android." \
  "@/android/KMEA" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  ":app                   KMSample1"

builder_parse "$@"

android_set_gradle_environment

builder_describe_outputs \
  build:app    "/android/Samples/KMSample1/app/build/outputs/apk/${BUILDER_CONFIGURATION}/KMSample1-${KEYMAN_VERSION_FOR_FILENAME}-${ARCHIVE_TARGET}.apk"

#### Build action definitions ####

do_build() {
  cp "$KEYMAN_ROOT/android/KMEA/app/build/outputs/aar/keyman-engine-${BUILDER_CONFIGURATION}.aar" "$KEYMAN_ROOT/android/Samples/KMSample1/app/libs/keyman-engine.aar"
  ./gradlew $GRADLE_DAEMON $GRADLE_TARGET
}

builder_run_action clean:app    rm -rf build app/build
builder_run_action build:app    do_build
