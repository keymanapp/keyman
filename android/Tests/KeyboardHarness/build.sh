#!/bin/bash
# Build KeyboardHarness test app


display_usage ( ) {
    echo "build.sh [-no-daemon] [-debug]"
    echo
    echo "Build KeyboardHarness test app"
    echo "  -no-daemon              Don't start the Gradle daemon. Use for CI"
    echo "  -debug                  Compile only Debug variant"
    exit 1
}

echo Build KeyboardHarness test app

#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0

echo Build KMEA first
cd ../../KMEA
./build.sh
cd ../../web/source
echo Compile KMW\'s keyboard automation interface
# Compiles the KMW recorder interface, which is very useful in test cases for automating the embedded keyboard.
./build_recorder.sh
cd ../../android/Tests/KeyboardHarness
cp ../keyman-engine.aar app/libs/

KMW_RECORDER_PATH=../../../web/release/recorder
KMW_RECORDER_FILE=recorder_InputEvents.js

ASSETS_PATH=app/src/main/assets

NO_DAEMON=false
ONLY_DEBUG=false

# Parse args
while [[ $# -gt 0 ]] ; do
    key="$1"
    case $key in
        -no-daemon)
            NO_DAEMON=true
            ;;
        -debug)
            ONLY_DEBUG=true
            ;;
        -h|-?)
            display_usage
            ;;
    esac
    shift # past argument
done

echo
echo "NO_DAEMON: $NO_DAEMON"
echo "ONLY_DEBUG: $ONLY_DEBUG"
echo

if [ "$NO_DAEMON" = true ]; then
  DAEMON_FLAG=--no-daemon
else
  DAEMON_FLAG=
fi

if [ "$ONLY_DEBUG" = true ]; then
  BUILD_FLAG=assembleDebug
else
  BUILD_FLAG=build
fi

echo Build KeyboardHarness
cp $KMW_RECORDER_PATH/$KMW_RECORDER_FILE $ASSETS_PATH
./gradlew $DAEMON_FLAG clean $BUILD_FLAG

