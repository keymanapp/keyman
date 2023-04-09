#!/usr/bin/env bash

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

# Please note that this build script (understandably) assumes that it is running on Mac OS X.
if [[ "${OSTYPE}" == *"darwin" ]]; then
  echo "This build script will only run in a Mac environment."
  exit 1
fi

if [ -z "$TARGET" ]; then
  exit 1
fi

display_usage ( ) {
    echo "build.sh [-no-update] | [-lib-build] | [-lib-ignore] | [-clean]"
    echo
    echo "  -clean          Removes all previously-existing build products for this project before building."
    echo "  -no-update      If an in-place copy of KeymanEngine.xcframework exists, does not seek out an updated copy."
    echo "  -lib-build      Actively rebuilds KMEI before copying its build products to project resources."
    echo "  -lib-nobuild    Prevents the build script from building KeymanEngine under any circumstances."
    echo "  -no-codesign    Performs the build without code signing."
    echo "  -debug          Sets the configuration to debug mode instead of release."
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

KMEI_BUILD_DIR="../../"

BUILD_FOLDER=build

do_clean ( ) {
  rm -rf $BUILD_FOLDER
}

### START OF THE BUILD ###

DO_UPDATE=true
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
        -h|-\?)
            display_usage
            ;;
        -clean)
            do_clean
            KMEI_FLAGS="$KMEI_FLAGS -clean"
            FORCE_KMEI_BUILD=true
            ;;
        -debug)
            CONFIG=Debug
            KMEI_FLAGS="$KMEI_FLAGS -debug"
            ;;
    esac
    shift
done

KEYMAN_ENGINE_FRAMEWORK_SRC="../../build/Build/Products/$CONFIG/KeymanEngine.xcframework"
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
        ./kmbuild.sh -only-framework $KMEI_FLAGS
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

if [ $CODE_SIGN = true ]; then
  run_xcodebuild -quiet -target "$TARGET" -config "$CONFIG"
else
  run_xcodebuild -quiet CODE_SIGN_IDENTITY="" CODE_SIGNING_REQUIRED=NO CODE_SIGNING_ALLOWED="NO" CODE_SIGNING_ENTITLEMENTS="" -target "$TARGET" -config "$CONFIG"
fi

if [ $? = 0 ]; then
  echo "Build complete."
fi
