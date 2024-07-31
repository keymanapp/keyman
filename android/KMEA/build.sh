#!/usr/bin/env bash
# Build Keyman Engine for Android using Keyman Web artifacts
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# ################################ Main script ################################

# Definition of global compile constants

KEYMAN_ANDROID_ROOT="$KEYMAN_ROOT/android"
KEYMAN_WEB_ROOT="$KEYMAN_ROOT/web"
ENGINE_ASSETS="$KEYMAN_ANDROID_ROOT/KMEA/app/src/main/assets"
CONFIG="release"
BUILD_FLAGS="aR -x lint -x test"           # Gradle build w/o test
TEST_FLAGS="-x aR lintRelease testRelease" # Gradle test w/o build
JUNIT_RESULTS="##teamcity[importData type='junit' path='keyman\android\KMEA\app\build\test-results\testReleaseUnitTest\']"

builder_describe "Builds Keyman Engine for Android." \
  "@/web/src/app/webview" \
  "@/common/web/sentry-manager" \
  "clean" \
  "configure" \
  "build" \
  "test             Runs lint and unit tests." \
  ":engine          Builds Engine" \
  "--ci             Don't start the Gradle daemon. For CI"

# parse before describe_outputs to check debug flags
builder_parse "$@"

if builder_is_debug_build; then
  builder_heading "### Debug config ####"
  CONFIG="debug"
  BUILD_FLAGS="assembleDebug -x lint -x test"
  TEST_FLAGS="-x assembleDebug lintDebug testDebug"
  JUNIT_RESULTS="##teamcity[importData type='junit' path='keyman\android\KMEA\app\build\test-results\testDebugUnitTest\']"
fi

builder_describe_outputs \
  build:engine     /android/KMEA/app/build/outputs/aar/keyman-engine-${CONFIG}.aar

#### Build


# Parse args

DAEMON_FLAG=
if builder_has_option --ci; then
  DAEMON_FLAG=--no-daemon
fi

#### Build action definitions ####

# Check about cleaning artifact paths
if builder_start_action clean:engine; then
  rm -rf "$KEYMAN_ROOT/android/KMEA/app/build/outputs"
  builder_finish_action success clean:engine
fi

if builder_start_action configure:engine; then

  builder_finish_action success configure:engine
fi

# Destinations that will need the keymanweb artifacts


if builder_start_action build:engine; then

  # Copy KeymanWeb artifacts
  echo "Copying Keyman Web artifacts"
  cp "$KEYMAN_WEB_ROOT/build/app/webview/$CONFIG/keymanweb-webview.js" "$ENGINE_ASSETS/keymanweb-webview.js"
  cp "$KEYMAN_WEB_ROOT/build/app/webview/$CONFIG/keymanweb-webview.js.map" "$ENGINE_ASSETS/keymanweb-webview.js.map"
  cp "$KEYMAN_WEB_ROOT/build/app/webview/$CONFIG/map-polyfill.js" "$ENGINE_ASSETS/map-polyfill.js"
  cp "$KEYMAN_WEB_ROOT/build/app/resources/osk/ajax-loader.gif" "$ENGINE_ASSETS/ajax-loader.gif"
  cp "$KEYMAN_WEB_ROOT/build/app/resources/osk/kmwosk.css" "$ENGINE_ASSETS/kmwosk.css"
  cp "$KEYMAN_WEB_ROOT/build/app/resources/osk/globe-hint.css" "$ENGINE_ASSETS/globe-hint.css"
  cp "$KEYMAN_WEB_ROOT/build/app/resources/osk/keymanweb-osk.ttf" "$ENGINE_ASSETS/keymanweb-osk.ttf"

  cp "$KEYMAN_ROOT/node_modules/@sentry/browser/build/bundle.min.js" "$ENGINE_ASSETS/sentry.min.js"
  cp "$KEYMAN_ROOT/common/web/sentry-manager/build/lib/index.js" "$ENGINE_ASSETS/keyman-sentry.js"

  echo "Copying es6-shim polyfill"
  cp "$KEYMAN_ROOT/node_modules/es6-shim/es6-shim.min.js" "$ENGINE_ASSETS/es6-shim.min.js"

  echo "BUILD_FLAGS $BUILD_FLAGS"
  # Build without test
  ./gradlew $DAEMON_FLAG clean $BUILD_FLAGS

  builder_finish_action success build:engine
fi

if builder_start_action test:engine; then

  if builder_has_option --ci; then
    # Report JUnit test results to CI
    echo "$JUNIT_RESULTS"
  fi

  echo "TEST_FLAGS: $TEST_FLAGS"
  ./gradlew $DAEMON_FLAG $TEST_FLAGS

  builder_finish_action success test:engine
fi
