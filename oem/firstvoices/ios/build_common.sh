#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# Include our resource functions; they're pretty useful!
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# Please note that this build script (understandably) assumes that it is running on Mac OS X.
if [[ "${OSTYPE}" == *"darwin" ]]; then
  echo "This build script will only run in a Mac environment."
  exit 1
fi

if [ -z "$TARGET" ]; then
  exit 1
fi

display_usage ( ) {
    echo "build.sh [-no-update] | [-no-archive] | [-lib-build] | [-lib-ignore] | [-clean]"
    echo
    echo "  -clean             Removes all previously-existing build products for this project before building."
    echo "  -no-update         If an in-place copy of KeymanEngine.framework exists, does not seek out an updated copy."
    echo "  -lib-build         Actively rebuilds KMEI before copying its build products to project resources."
    echo "  -lib-nobuild       Prevents the build script from building KeymanEngine under any circumstances."
    echo "  -no-carthage       Disables downloading and building for dependencies."
    echo "  -no-codesign       Performs the build without code signing."
    echo "  -debug             Sets the configuration to debug mode instead of release."
    echo "  -add-sim-artifact  Also produce a Simulator-installable .app build artifact"
    echo
    echo "  If no settings are specified this script will grab a copy of the most recent build of KeymanEngine,"
    echo "  performing an initial build of it if necessary."
    exit 1
}

assert ( ) {
    if ! [ -f $1 ]; then
        echo "Build failed:  missing $1"
        exit 1
    fi
}

verify_KMEI ( ) {
    KMEI_BUILD_EXISTS=true
    [ -d "$KEYMAN_ENGINE_FRAMEWORK_SRC" ] || KMEI_BUILD_EXISTS=false
}

KMEI_BUILD_DIR="$KEYMAN_ROOT/ios/"

BUILD_FOLDER=build

do_clean ( ) {
  rm -rf $BUILD_FOLDER
  rm -rf Carthage
}

### START OF THE BUILD ###

DO_UPDATE=true
DO_ARCHIVE=true
DO_SIMULATOR_TARGET=false
DO_CARTHAGE=true
FORCE_KMEI_BUILD=false
ALLOW_KMEI_BUILD=true
CODE_SIGN=true
CONFIG=Release
KMEI_FLAGS=

while [[ $# -gt 0 ]] ; do
    key="$1"
    case $key in
        -no-update)
            DO_UPDATE=false
            ALLOW_KMEI_BUILD=false
            ;;
        -lib-build)
            FORCE_KMEI_BUILD=true
            ;;
        -lib-nobuild)
            ALLOW_KMEI_BUILD=false
            ;;
        -no-codesign)
            CODE_SIGN=false
            KMEI_FLAGS="$KMEI_FLAGS -no-codesign"
            ;;
        -no-carthage)
            DO_CARTHAGE=false
            KMEI_FLAGS="$KMEI_FLAGS -no-carthage"
            ;;
        -no-archive)
            DO_ARCHIVE=false
            ;;
        -h|-\?)
            display_usage
            ;;
        -clean)
            do_clean
            KMEI_FLAGS="$KMEI_FLAGS -clean"
            # FORCE_KMEI_BUILD=true
            ;;
        -debug)
            CONFIG=Debug
            KMEI_FLAGS="$KMEI_FLAGS -debug"
            ;;
        -add-sim-artifact)
            DO_SIMULATOR_TARGET=true
            ;;
    esac
    shift
done

#
# Build Keyman Engine
#

if [ $CONFIG = "Debug" ]; then
    # When doing a debug run, we'd like to be able to run the result in the Simulator.
    # 'universal' can be run both on real devices and in the simulator.
    PLATFORM_TARGET="universal"
else
    # Actual release builds can't have Simulator-targeted content; Apple will block the submission.
    PLATFORM_TARGET="iphoneos"
fi

KEYMAN_ENGINE_FRAMEWORK_SRC="$KMEI_BUILD_DIR/build/Build/Products/$CONFIG/KeymanEngine.xcframework"
KEYMAN_ENGINE_FRAMEWORK_DST=./

if [ $DO_UPDATE = true ]; then
    # Does a prior build of KMEI exist?
    verify_KMEI

    if [ $ALLOW_KMEI_BUILD = true ] && [ $FORCE_KMEI_BUILD = false ] && [ $KMEI_BUILD_EXISTS = false ]; then
        echo "Previous KeymanEngine build information is unavailable."
        FORCE_KMEI_BUILD=true
    fi

    if [ $FORCE_KMEI_BUILD = true ]; then
        echo "Building KeymanEngine..."
        base_dir="$(pwd)"

        cd $KMEI_BUILD_DIR
        ./build.sh -only-framework $KMEI_FLAGS
        cd $base_dir
    fi

    verify_KMEI

    if ! [ $KMEI_BUILD_EXISTS ]; then
      echo "Build failed:  could not build required KeymanEngine resources."
      exit 1
    fi

    # Copy resources.
    /bin/cp -Rf "$KEYMAN_ENGINE_FRAMEWORK_SRC" "$KEYMAN_ENGINE_FRAMEWORK_DST"
fi

# First things first - update our dependencies.

if [ $DO_CARTHAGE = true ]; then
  echo
  echo "Load dependencies with Carthage"

  carthage checkout || fail "Carthage dependency loading failed"

  # --no-use-binaries: due to https://github.com/Carthage/Carthage/issues/3134,
  # which affects the sentry-cocoa dependency.
  carthage build --use-xcframeworks --no-use-binaries --platform iOS || fail "Carthage dependency loading failed"
fi

#
# Build Target Application
#

DERIVED_DATA=build
BUILD_PATH=$DERIVED_DATA/Build/Products
XCODEFLAGS="-quiet -configuration $CONFIG"
XCODEFLAGS_EXT="$XCODEFLAGS -derivedDataPath $DERIVED_DATA" # -workspace $TARGET.xcworkspace"
ARCHIVE_PATH=$BUILD_PATH/${CONFIG}-iphoneos/$TARGET.xcarchive

if [ $CODE_SIGN = true ]; then

  if [ $DO_ARCHIVE = true ]; then
    # Time to prepare the deployment archive data.
    echo ""
    echo "Preparing .ipa file for deployment to real devices."
    run_xcodebuild $XCODEFLAGS_EXT -scheme $TARGET -archivePath $ARCHIVE_PATH archive -allowProvisioningUpdates \
               VERSION=$VERSION \
               VERSION_WITH_TAG=$VERSION_WITH_TAG

    assertDirExists "$ARCHIVE_PATH"


    # Do NOT use the _EXT variant here; there's no scheme to ref, which will lead
    # Xcode to generate a build error.
    run_xcodebuild $XCODEFLAGS -exportArchive -archivePath $ARCHIVE_PATH -exportOptionsPlist exportAppStore.plist \
               -exportPath $BUILD_PATH/${CONFIG}-iphoneos -allowProvisioningUpdates \
               VERSION=$VERSION \
               VERSION_WITH_TAG=$VERSION_WITH_TAG
  else
    run_xcodebuild $XCODEFLAGS_EXT -scheme "$TARGET" \
               VERSION=$VERSION \
               VERSION_WITH_TAG=$VERSION_WITH_TAG
  fi
else
  run_xcodebuild CODE_SIGN_ENTITLEMENTS="" CODE_SIGNING_ALLOWED="NO" CODE_SIGN_IDENTITY="" CODE_SIGNING_REQUIRED=NO \
             $XCODEFLAGS_EXT -scheme "$TARGET" \
             VERSION=$VERSION \
             VERSION_WITH_TAG=$VERSION_WITH_TAG
fi

if [ $DO_SIMULATOR_TARGET == true ]; then
  echo "Preparing .app file as Simulator-targeted build artifact."
  run_xcodebuild CODE_SIGN_ENTITLEMENTS="" CODE_SIGNING_ALLOWED="NO" CODE_SIGN_IDENTITY="" CODE_SIGNING_REQUIRED=NO \
             $XCODEFLAGS_EXT -scheme "$TARGET" \
             -sdk iphonesimulator \
             VERSION=$VERSION \
             VERSION_WITH_TAG=$VERSION_WITH_TAG
fi

if [ $? = 0 ]; then
  echo "Build complete."
fi
