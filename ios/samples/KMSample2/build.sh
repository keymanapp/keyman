#!/bin/sh

# Please note that this build script (understandably) assumes that it is running on Mac OS X.
if [[ "${OSTYPE}" == *"darwin" ]]; then
  echo "This build script will only run in a Mac environment."
  exit 1
fi

display_usage ( ) {
    echo "build.sh [-no-update] | [-lib-build] | [-lib-ignore] | [-clean]"
    echo
    echo "  -clean          Removes all previously-existing build products for this project before building."
    echo "  -no-update      If an in-place copy of libKeyman exists, does not seek out an updated copy."
    echo "  -lib-build      Actively rebuilds KMEI before copying its build products to project resources."
    echo "  -lib-nobuild    Prevents the build script from building libKeyman under any circumstances."
    echo "  -no-codesign    Performs the build without code signing."
    echo
    echo "  If no settings are specified this script will grab a copy of the most recent build of libKeyman,"
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
    [ -d "$LIB_KEYMAN_SRC" ] || KMEI_BUILD_EXISTS=false
}

LIB_KEYMAN_SRC=../../engine/KMEI/build/Debug-universal/KeymanEngine.framework
LIB_KEYMAN_DEST_DIR=./

KMEI_BUILD_DIR="../../"
KMEI_BUILD="./build.sh -libKeyman"

BUILD_FOLDER=build

do_clean ( ) {
  rm -rf $BUILD_FOLDER
}

### START OF THE BUILD ###

DO_UPDATE=true
FORCE_KMEI_BUILD=false
ALLOW_KMEI_BUILD=true
CODE_SIGN=true

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
            ;;
        -h|-?)
            display_usage
            ;;
        -clean)
            do_clean
            ;;
    esac
    shift
done

if [ $DO_UPDATE = true ]; then
    # Does a prior build of KMEI exist?
    verify_KMEI

    if [ $ALLOW_KMEI_BUILD = true ] && [ $FORCE_KMEI_BUILD = false ] && [ $KMEI_BUILD_EXISTS = false ]; then
        echo "Previous libKeyman build information is unavailable."
        FORCE_KMEI_BUILD=true
    fi

    if [ $FORCE_KMEI_BUILD = true ]; then
        echo "Building libKeyman..."
        base_dir="$(pwd)"

        cd $KMEI_BUILD_DIR
        $KMEI_BUILD
        cd $base_dir
    fi

    verify_KMEI

    if ! [ $KMEI_BUILD_EXISTS ]; then
      echo "Build failed:  could not build required libKeyman resources."
      exit 1
    fi

    # Copy resources.
    cp -Rf "$LIB_KEYMAN_SRC" "$LIB_KEYMAN_DEST"
fi

if [ $CODE_SIGN = true ]; then
  xcodebuild -quiet -target KMSample2
else
  xcodebuild -quiet CODE_SIGN_IDENTITY="" CODE_SIGNING_REQUIRED=NO -target KMSample2
fi

if [ $? = 0 ]; then
  echo "Build complete."
fi
