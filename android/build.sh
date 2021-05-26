#!/bin/bash
# Build Keyman Engine Android and KMAPro

# Set sensible script defaults:
# set -e: Terminate script if a command returns an error
set -e
# set -u: Terminate script if an unset variable is used
#set -u: Not set because of $RELEASE_OEM
# set -x: Debugging use, print each statement
# set -x

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0

clean ( ) {
  if [ -f "$KEYMAN_ROOT/android/KMAPro/kMAPro/libs/keyman-engine.aar" ]; then
    echo "Cleaning keyman-engine.aar"
    rm -rf "$KEYMAN_ROOT/android/KMAPro/kMAPro/libs/keyman-engine.aar"
  fi
  if [ -d "$KEYMAN_ROOT/android/KMAPro/kMAPro/build/outputs" ]; then
    echo "Cleaning KMAPro build outputs"
    rm -rf "$KEYMAN_ROOT/android/KMAPro/kMAPro/build/outputs"
  fi
  if [ -d "$KEYMAN_ROOT/android/upload" ]; then
    echo "Cleaning upload directory"
    rm -rf "$KEYMAN_ROOT/android/upload"
  fi
}

echo Build KMEA and KMAPro:

# Check about cleaning artifact paths
if [[ "$1" == "-clean" ]] ; then
  clean
  shift
fi

# Building Keyman Engine for Android

cd "$KEYMAN_ROOT/android/KMEA"
./build.sh "$@"

if [ $? -ne 0 ]; then
    die "ERROR: KMEA/build.sh failed"
fi

# Building Keyman for Android

cd "$KEYMAN_ROOT/android/KMAPro"
./build.sh "$@"

if [ $? -ne 0 ]; then
    die "ERROR: KMAPro/build.sh failed"
fi

cd "$KEYMAN_ROOT/android"

# Building OEM apps

if [ ! -z "$RELEASE_OEM" ]; then
  pushd "$KEYMAN_ROOT/oem/firstvoices/android"
  ./build.sh -download-keyboards -lib-nobuild "$@"

  if [ $? -ne 0 ]; then
    die "ERROR: oem/firstvoices/android/build.sh failed"
  fi
fi
