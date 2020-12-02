#!/bin/bash
# CI script to publish specified app APKs to the Play Store.
# The APKs should already have been built from a separate script

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
. "$(dirname "$THIS_SCRIPT")/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

echo Publishing APKs to Play Store

#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0

display_usage ( ) {
  echo "build-publish.sh [-no-daemon] [-kmapro] [-fv]"
  echo
  echo "Publish app to the Play Store"
  echo "  -no-daemon              Don't start the Gradle daemon. Use for CI"
  echo "  -kmapro                 Keyman for Android"
  echo "  -fv                     First Voices"
  exit 1
}

NO_DAEMON=false
DO_KMAPRO=false
DO_FV=false

# Parse args
while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    -no-daemon)
        NO_DAEMON=true
        ;;
    -kmapro)
        DO_KMAPRO=true
        ;;
    -fv)
        DO_FV=true
        ;;
    -h|-?)
        display_usage
        ;;
  esac
  shift # past argument
done

echo
echo "NO_DAEMON: $NO_DAEMON"
echo "DO_KMAPRO: $DO_KMAPRO"
echo "DO_FV: $DO_FV"
echo

if [ "$NO_DAEMON" = true ]; then
  DAEMON_FLAG=--no-daemon
else
  DAEMON_FLAG=
fi

BUILD_FLAGS="publishReleaseApk"
echo "BUILD_FLAGS $BUILD_FLAGS"

# Publish Keyman for Android
if [ "$DO_KMAPRO" = true ]; then
  cd "$KEYMAN_ROOT/android/KMAPro/"
  ./gradlew $DAEMON_FLAG $BUILD_FLAGS
fi

# Publish FV app
if [ "$DO_FV" = true ]; then
  cd "$KEYMAN_ROOT/oem/firstvoices/android/"
  ./gradlew $DAEMON_FLAG $BUILD_FLAGS
fi
