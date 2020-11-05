#!/bin/bash
# Build Keyman Engine Android using Keyman Web artifacts
#
# Abbreviations:
# KMA  - Keyman Android
# KMEA - Keyman Engine Android
# KMW  - Keyman Web

# Set sensible script defaults:
# set -e: Terminate script if a command returns an error
set -e
# set -u: Terminate script if an unset variable is used
set -u
# set -x: Debugging use, print each statement
# set -x

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

display_usage ( ) {
    echo "build.sh [-no-kmw-build] | [-no-kmw] [-no-daemon] | [-no-test] | [-upload-sentry] | [-debug]"
    echo
    echo "Build Keyman Engine Android (KMEA) using Keyman Web (KMW) artifacts"
    echo "  -no-kmw-build           Don't build KMW. Just copy existing artifacts"
    echo "  -no-kmw                 Don't build KMW. Don't copy artifacts"
    echo "  -no-daemon              Don't start the Gradle daemon. Use for CI"
    echo "  -no-test                Don't run the unit-test suite.  Use for development builds"
    echo "                          to facilitate manual debugging and testing"
    echo "  -upload-sentry          Uploads debug symbols, etc, to Sentry"
    echo "  -debug                  Local debug build; use for development builds"
    exit 1
}

echo Build KMEA

#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0

# Path definitions

KMA_ROOT="$KEYMAN_ROOT/android"
KMW_ROOT="$KEYMAN_ROOT/web"
KMW_SOURCE="$KMW_ROOT/source"
KMEA_ASSETS="$KMA_ROOT/KMEA/app/src/main/assets"

warn ( ) {
    echo "$*"
}

die ( ) {
    echo
    echo "$*"
    echo
    exit 1
}

# Default is building KMW and copying artifacts
DO_BUILD=true
DO_COPY=true
DO_TEST=true
NO_DAEMON=false
DEBUG_BUILD=false
KMWFLAGS=-embed
KMW_PATH=

# Parse args
while [[ $# -gt 0 ]] ; do
    key="$1"
    case $key in
        -no-kmw-build)
            DO_BUILD=false
            DO_COPY=true
            ;;
        -no-kmw)
            DO_BUILD=false
            DO_COPY=false
            ;;
        -no-daemon)
            NO_DAEMON=true
            ;;
        -upload-sentry)
            # Overrides default set by build-utils.sh
            UPLOAD_SENTRY=true
            ;;
        -debug)
            DEBUG_BUILD=true
            KMWFLAGS=-debug_embedded
            KMW_PATH=unminified
            ;;
        -h|-?)
            display_usage
            ;;
        -no-test)
            DO_TEST=false
            ;;
    esac
    shift # past argument
done

# Local development optimization - cross-target Sentry uploading when requested
# by developer. As it's not CI, the Web artifacts won't exist otherwise...
# unless the developer manually runs the correct build configuration accordingly.
if [[ $VERSION_ENVIRONMENT == "local" ]] && [[ $UPLOAD_SENTRY == true ]]; then
    KMWFLAGS="$KMWFLAGS -upload-sentry"
fi

echo
echo "DO_BUILD: $DO_BUILD"
echo "DO_COPY: $DO_COPY"
echo "DO_TEST: $DO_TEST"
echo "NO_DAEMON: $NO_DAEMON"
echo "DEBUG_BUILD: $DEBUG_BUILD"
echo "KMWFLAGS: $KMWFLAGS"
echo "KMW_PATH: $KMW_PATH"
echo

if [ "$NO_DAEMON" = true ]; then
  DAEMON_FLAG=--no-daemon
else
  DAEMON_FLAG=
fi

# Destinations that will need the keymanweb artifacts

PLATFORM=`uname -s`

if [ "$DO_BUILD" = true ]; then
    echo "Building keyman web engine"
    cd $KMW_SOURCE

    ./build.sh $KMWFLAGS

    if [ $? -ne 0 ]; then
        die "ERROR: keymanweb build failed. Exiting"
    fi
fi
if [ "$DO_COPY" = true ]; then
    echo "Copying KMW artifacts"
    cp $KMW_ROOT/release/$KMW_PATH/embedded/resources/osk/ajax-loader.gif $KMEA_ASSETS/ajax-loader.gif
    cp $KMW_ROOT/release/$KMW_PATH/embedded/keyman.js $KMEA_ASSETS/keymanandroid.js
    cp $KMW_ROOT/release/$KMW_PATH/embedded/keyman.js.map $KMEA_ASSETS/keyman.js.map
    cp $KMW_ROOT/release/$KMW_PATH/embedded/resources/osk/kmwosk.css $KMEA_ASSETS/kmwosk.css
    cp $KMW_ROOT/release/$KMW_PATH/embedded/resources/osk/keymanweb-osk.eot $KMEA_ASSETS/keymanweb-osk.eot
    cp $KMW_ROOT/release/$KMW_PATH/embedded/resources/osk/keymanweb-osk.ttf $KMEA_ASSETS/keymanweb-osk.ttf
    cp $KMW_ROOT/release/$KMW_PATH/embedded/resources/osk/keymanweb-osk.woff $KMEA_ASSETS/keymanweb-osk.woff

    cp $KMW_ROOT/node_modules/@keymanapp/web-sentry-manager/dist/index.js $KMEA_ASSETS/keyman-sentry.js

    if [ $? -ne 0 ]; then
        die "ERROR: copying artifacts failed"
    fi
fi

echo "Gradle Build of KMEA"
cd $KMA_ROOT/KMEA

if [ "$DEBUG_BUILD" = true ]; then
  BUILD_FLAGS="assembleDebug lintDebug"
  TEST_FLAGS="testDebug"
  ARTIFACT="app-debug.aar"
  if [ "$DO_TEST" = true ]; then
    # Report JUnit test results to CI
    echo "##teamcity[importData type='junit' path='keyman\android\KMEA\app\build\test-results\testDebugUnitTest\']"
  fi
else
  BUILD_FLAGS="aR lint"
  TEST_FLAGS="testRelease"
  ARTIFACT="app-release.aar"
  if [ "$DO_TEST" = true ]; then
    # Report JUnit test results to CI
    echo "##teamcity[importData type='junit' path='keyman\android\KMEA\app\build\test-results\testReleaseUnitTest\']"
  fi
fi

echo "BUILD_FLAGS $BUILD_FLAGS"
./gradlew $DAEMON_FLAG clean $BUILD_FLAGS
if [ $? -ne 0 ]; then
    die "ERROR: Build of KMEA failed"
fi
if [ "$DO_TEST" = true ]; then
    echo "TEST_FLAGS $TEST_FLAGS"
    ./gradlew $DAEMON_FLAG $TEST_FLAGS
    if [ $? -ne 0 ]; then
        die "ERROR: KMEA test cases failed"
    fi
fi

echo "Copying Keyman Engine for Android to KMAPro, Sample apps, and Tests"
mv $KMA_ROOT/KMEA/app/build/outputs/aar/$ARTIFACT $KMA_ROOT/KMAPro/kMAPro/libs/keyman-engine.aar
cp $KMA_ROOT/KMAPro/kMAPro/libs/keyman-engine.aar $KMA_ROOT/Samples/KMSample1/app/libs/keyman-engine.aar
cp $KMA_ROOT/KMAPro/kMAPro/libs/keyman-engine.aar $KMA_ROOT/Samples/KMSample2/app/libs/keyman-engine.aar
cp $KMA_ROOT/KMAPro/kMAPro/libs/keyman-engine.aar $KMA_ROOT/Tests/KeyboardHarness/app/libs/keyman-engine.aar

cd ..\
