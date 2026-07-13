#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# Build Keyman Engine for Android using Keyman Web artifacts
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE


# ################################ Main script ################################

builder_describe "Builds Keyman Engine for Android." \
  "@/web/src/app/webview" \
  "@/common/web/sentry-manager" \
  "clean" \
  "configure" \
  "build" \
  "test             Runs lint and unit tests." \
  ":engine          Builds Engine"

builder_parse "$@"

# Definition of global compile constants

if builder_is_debug_build; then
  BUILD_FLAGS="assembleDebug -x lintDebug -x testDebug"
  TEST_FLAGS="-x assembleDebug lintDebug testDebug"
  JUNIT_RESULTS="##teamcity[importData type='junit' path='keyman\android\KMEA\app\build\test-results\testDebugUnitTest\']"
else
  BUILD_FLAGS="assembleRelease -x lint -x test"           # Gradle build w/o test
  TEST_FLAGS="-x assembleRelease lintRelease testRelease" # Gradle test w/o build
  JUNIT_RESULTS="##teamcity[importData type='junit' path='keyman\android\KMEA\app\build\test-results\testReleaseUnitTest\']"
fi

if builder_is_ci_build; then
  GRADLE_DAEMON=--no-daemon
else
  GRADLE_DAEMON=
fi

builder_describe_outputs \
  build:engine     /android/KMEA/app/build/outputs/aar/keyman-engine-${BUILDER_CONFIGURATION}.aar

#### Build action definitions ####

do_build() {
  local ENGINE_ASSETS="$KEYMAN_ROOT/android/KMEA/app/src/main/assets"

  # Copy KeymanWeb artifacts
  builder_echo "Copying Keyman Web artifacts"
  cp "$KEYMAN_ROOT/web/build/app/webview/$BUILDER_CONFIGURATION/keymanweb-webview.js" "$ENGINE_ASSETS/"
  cp "$KEYMAN_ROOT/web/build/app/webview/$BUILDER_CONFIGURATION/keymanweb-webview.js.map" "$ENGINE_ASSETS/"
  cp "$KEYMAN_ROOT/web/build/app/webview/$BUILDER_CONFIGURATION/map-polyfill.js" "$ENGINE_ASSETS/"
  cp "$KEYMAN_ROOT/web/build/app/resources/osk/ajax-loader.gif" "$ENGINE_ASSETS/"
  cp "$KEYMAN_ROOT/web/build/app/resources/osk/kmwosk.css" "$ENGINE_ASSETS/"
  cp "$KEYMAN_ROOT/web/build/app/resources/osk/globe-hint.css" "$ENGINE_ASSETS/"
  cp "$KEYMAN_ROOT/web/build/app/resources/osk/keymanweb-osk.ttf" "$ENGINE_ASSETS/"
  cp "$KEYMAN_ROOT/common/web/sentry-manager/build/lib/index.js" "$ENGINE_ASSETS/keyman-sentry.js"

  builder_echo "Copying es6-shim polyfill"
  cp "$KEYMAN_ROOT/node_modules/es6-shim/es6-shim.min.js" "$ENGINE_ASSETS/"

  builder_echo "BUILD_FLAGS $BUILD_FLAGS"

  # Build without test
  ./gradlew $GRADLE_DAEMON $BUILD_FLAGS
}

do_test() {
  if builder_is_ci_build; then
    # Report JUnit test results to CI
    echo "$JUNIT_RESULTS"
  fi

  builder_echo "TEST_FLAGS: $TEST_FLAGS"
  ./gradlew $GRADLE_DAEMON $TEST_FLAGS
}

builder_run_action clean:engine    rm -rf build app/build
builder_run_action build:engine    do_build
builder_run_action test:engine     do_test
