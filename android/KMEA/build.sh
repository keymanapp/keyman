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

#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0

# Path definitions
KM_ROOT="$(cd "$( dirname "${BASH_SOURCE[0]}" )/../.." && pwd )"
KMA_ROOT="$KM_ROOT/android"
KMW_ROOT="$KM_ROOT/web"
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

# Report JUnit test results to CI
echo "##teamcity[importData type='junit' path='keyman\android\KMEA\app\build\test-results\testReleaseUnitTest\']"

if [ "$DO_BUILD" = true ]; then
    echo "Building keyman web engine"
    cd $KMW_SOURCE

    ./build.sh -embed
	
    if [ $? -ne 0 ]; then
        die "ERROR: keymanweb build failed. Exiting"
    fi
fi
if [ "$DO_COPY" = true ]; then
    echo "Copying KMW artifacts"
    cp $KMW_ROOT/release/unminified/embedded/resources/osk/ajax-loader.gif $KMEA_ASSETS/ajax-loader.gif
    cp $KMW_ROOT/release/unminified/embedded/keyman.js $KMEA_ASSETS/keyman.js
    cp $KMW_ROOT/release/unminified/embedded/resources/osk/kmwosk.css $KMEA_ASSETS/kmwosk.css
    cp $KMW_ROOT/release/unminified/embedded/resources/osk/keymanweb-osk.eot $KMEA_ASSETS/keymanweb-osk.eot
    cp $KMW_ROOT/release/unminified/embedded/resources/osk/keymanweb-osk.ttf $KMEA_ASSETS/keymanweb-osk.ttf
    cp $KMW_ROOT/release/unminified/embedded/resources/osk/keymanweb-osk.woff $KMEA_ASSETS/keymanweb-osk.woff
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
./gradlew $DAEMON_FLAG test
if [ $? -ne 0 ]; then
    die "ERROR: KMEA test cases failed"
fi

echo "Copying Keyman Engine for Android to KMAPro, Sample apps, and Tests"
mv $KMA_ROOT/KMEA/app/build/outputs/aar/app-release.aar $KMA_ROOT/KMAPro/kMAPro/libs/keyman-engine.aar
cp $KMA_ROOT/KMAPro/kMAPro/libs/keyman-engine.aar $KMA_ROOT/Samples/KMSample1/app/libs/keyman-engine.aar
cp $KMA_ROOT/KMAPro/kMAPro/libs/keyman-engine.aar $KMA_ROOT/Samples/KMSample2/app/libs/keyman-engine.aar
cp $KMA_ROOT/KMAPro/kMAPro/libs/keyman-engine.aar $KMA_ROOT/Tests/keyman-engine.aar

cd ..\
