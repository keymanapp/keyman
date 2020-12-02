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

echo Build KMEA and KMAPro:

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
