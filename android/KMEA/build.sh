#!/bin/bash
# Build Keyman Engine Android using Keyman Web artifacts
#
# Abbreviations:
# KMA  - Keyman Android
# KMEA - Keyman Engine Android
# KMW  - Keyman Web

display_usage ( ) {
    echo "build.sh [-no-kmw-build] | [-no-kmw] [-no-daemon]"
    echo
    echo "Build Keyman Engine Android (KMEA) using Keyman Web (KMW) artifacts"
    echo "  -no-kmw-build           Don't build KMW. Just copy existing artifacts"
    echo "  -no-kmw                 Don't build KMW. Don't copy artifacts"
    echo "  -no-daemon              Don't start the Gradle daemon. Use for CI"
    exit 1
}

echo Build KMEA

# Path definitions
KMA_ROOT=`readlink -f ../`
KMW_ROOT=`readlink -f ../../web`
KMW_SOURCE="$KMW_ROOT/source"
KMEA_ASSETS="$KMA_ROOT/KMEA/app/src/main/assets"

# Default KMW build artifacts that get copied for KMEA
declare -A KMW_ARTIFACTS
KMW_ARTIFACTS=( \
    ["KEYMAN_JS"]=\
"$KMW_ROOT/embedded/keyman.js" \
    ["AJAX_LOADER"]=\
"$KMW_ROOT/embedded/resources/osk/ajax-loader.gif" \
    ["KMWOSK_CSS"]=\
"$KMW_ROOT/embedded/resources/osk/kmwosk.css" \
    ["KEYMANWEB_OSK_EOT"]=\
"$KMW_ROOT/embedded/resources/osk/keymanweb-osk.eot" \
    ["KEYMANWEB_OSK_TTF"]=\
"$KMW_ROOT/embedded/resources/osk/keymanweb-osk.ttf" \
    ["KEYMANWEB_OSK_WOFF"]=\
"$KMW_ROOT/embedded/resources/osk/keymanweb-osk.woff")

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
NO_DAEMON=false

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
        -h|-?)
            display_usage
            ;;
    esac
    shift # past argument
done

echo
echo "DO_BUILD: $DO_BUILD"
echo "DO_COPY: $DO_COPY"
echo "NO_DAEMON: $NO_DAEMON"
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
    # $OS should only be defined on Windows
    if [ "$OS" = "Windows_NT" ]; then
        ./build.bat -embed
    else
        ./build.sh -embed
    fi
    if [ $? -ne 0 ]; then
        die "ERROR: keymanweb build failed. Exiting"
    fi
fi
if [ "$DO_COPY" = true ]; then
    echo "Copying KMW artifacts"
    cp ${KMW_ARTIFACTS[AJAX_LOADER]} $KMEA_ASSETS/ajax-loader.gif
    cp ${KMW_ARTIFACTS[KEYMAN_JS]} $KMEA_ASSETS/keyman.js
    cp ${KMW_ARTIFACTS[KMWOSK_CSS]} $KMEA_ASSETS/kmwosk.css
    cp ${KMW_ARTIFACTS[KEYMANWEB_OSK_EOT]} $KMEA_ASSETS/keymanweb-osk.eot
    cp ${KMW_ARTIFACTS[KEYMANWEB_OSK_TTF]} $KMEA_ASSETS/keymanweb-osk.ttf
    cp ${KMW_ARTIFACTS[KEYMANWEB_OSK_WOFF]} $KMEA_ASSETS/keymanweb-osk.woff
    if [ $? -ne 0 ]; then
        die "ERROR: copying artifacts failed"
    fi   
fi

echo "Gradle Build of KMEA"
cd $KMA_ROOT/KMEA
./gradlew $DAEMON_FLAG clean
./gradlew $DAEMON_FLAG aR
if [ $? -ne 0 ]; then
    die "ERROR: Build of KMEA failed"
fi

echo "Copying Keyman Engine for Android to KMAPro and Sample apps"
mv $KMA_ROOT/KMEA/app/build/outputs/aar/app-release.aar $KMA_ROOT/KMAPro/kMAPro/libs/keyman-engine.aar 
cp $KMA_ROOT/KMAPro/kMAPro/libs/keyman-engine.aar $KMA_ROOT/Samples/KMSample1/app/libs/keyman-engine.aar
cp $KMA_ROOT/KMAPro/kMAPro/libs/keyman-engine.aar $KMA_ROOT/Samples/KMSample2/app/libs/keyman-engine.aar

cd ..\
