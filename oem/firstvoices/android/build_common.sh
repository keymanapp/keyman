#!/bin/bash

# Set sensible script defaults:
# set -e: Terminate script if a command returns an error
set -e
# set -u: Terminate script if an unset variable is used
set -u
# set -x: Debugging use, print each statement
# set -x

if [ -z "$TARGET" ]; then
  exit 1
fi

display_usage ( ) {
    echo "build_common.sh [-no-daemon] [-debug] [-no-update] [-lib-build|-no-lib-build]"
    echo "Build $TARGET"
    echo "  -no-daemon              Don't start the Gradle daemon. Use for CI"
    echo "  -debug                  Compile only Debug variant"
    echo "  -no-update              Don't copy or build the Keyman Engine library in (assumes already present)"
    echo "  -lib-build              Force rebuild of the Keyman Engine library"
    echo "  -no-lib-build           Only rebuild the Keyman Engine library if it doesn't exist in /android"
    exit 1
}

verify_KMEA ( ) {
    KMEA_BUILD_EXISTS=true
    [ -f "$KEYMAN_ENGINE_DST" ] || KMEA_BUILD_EXISTS=false
}

echo Build $TARGET

#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0

DO_UPDATE=true
FORCE_KMEA_BUILD=false
ALLOW_KMEA_BUILD=true
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
        # Settings relating to engine build
        -no-update)
            DO_UPDATE=false
            ALLOW_KMEA_BUILD=false
            ;;
        -lib-build)
            FORCE_KMEA_BUILD=true
            ;;
        -lib-nobuild|-no-lib-build)
            ALLOW_KMEA_BUILD=false
            ;;
        -h|-\?)
            display_usage
            ;;
    esac
    shift # past argument
done

echo
echo "NO_DAEMON: $NO_DAEMON"
echo "ONLY_DEBUG: $ONLY_DEBUG"
echo "DO_UPDATE: $DO_UPDATE"
echo "ALLOW_KMEA_BUILD: $ALLOW_KMEA_BUILD"
echo "FORCE_KMEA_BUILD: $FORCE_KMEA_BUILD"
echo

# The KMEA build script moves the final .aar from KMEA to KMAPro...
KMEA_BUILD_DIR=../../../android/KMEA/
KEYMAN_ENGINE_SRC=../../../android/KMAPro/kMAPro/libs/keyman-engine.aar
KEYMAN_ENGINE_DST=app/libs/keyman-engine.aar

#
# Build Keyman Engine
#

if [ $DO_UPDATE = true ]; then
    # Does a prior build of KMEA exist?
    verify_KMEA

    if [ $ALLOW_KMEA_BUILD = true ] && [ $FORCE_KMEA_BUILD = false ] && [ $KMEA_BUILD_EXISTS = false ]; then
        echo "Previous Keyman Engine build information is unavailable; rebuilding."
        FORCE_KMEA_BUILD=true
    fi

    if [ $FORCE_KMEA_BUILD = true ]; then
        echo "Building Keyman Engine..."
        pushd $KMEA_BUILD_DIR
        ./build.sh "$@"
        popd
    fi

    verify_KMEA

    if ! [ $KMEA_BUILD_EXISTS ]; then
      echo "Build failed: Could not build required Keyman Engine resources."
      exit 1
    fi

    # Copy Keyman Engine aar to
    cp "$KEYMAN_ENGINE_SRC" "$KEYMAN_ENGINE_DST"
fi


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

./gradlew $DAEMON_FLAG clean $BUILD_FLAG
