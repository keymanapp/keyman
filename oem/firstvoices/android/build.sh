#!/bin/bash

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
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/build-download-resources.sh"

display_usage ( ) {
  echo "build.sh [-no-daemon] [-debug] [-no-update] [-lib-build|-no-lib-build] [-download-keyboards] [-h|-?]"
  echo "Build $TARGET"
  echo "  -no-daemon              Don't start the Gradle daemon. Use for CI"
  echo "  -debug                  Compile only Debug variant"
  echo "  -no-update              Don't copy or build the Keyman Engine library in (assumes already present)"
  echo "  -lib-build              Force rebuild of the Keyman Engine library"
  echo "  -no-lib-build           Only rebuild the Keyman Engine library if it doesn't exist in /android"
  echo "  -download-resources     Download fv_all.kmp from downloads.keyman.com"
  echo "                          (dictionaries will be downloaded within the app)"
  echo ""
  exit 1
}

export TARGET=FirstVoices
KEYBOARD_PACKAGE_ID="fv_all"
KEYBOARDS_TARGET="$KEYMAN_ROOT/oem/firstvoices/android/app/src/main/assets/${KEYBOARD_PACKAGE_ID}.kmp"
KEYBOARDS_CSV="$KEYMAN_ROOT/oem/firstvoices/keyboards.csv"
KEYBOARDS_CSV_TARGET="$KEYMAN_ROOT/oem/firstvoices/android/app/src/main/assets/keyboards.csv"

# This build script assumes that the https://github.com/keymanapp/keyboards repo is in
# the same parent folder as this repo, with the default name 'keyboards'

PARAM_DEBUG=
PARAM_NO_DAEMON=
PARAM_NO_UPDATE=
PARAM_LIB_BUILD=
PARAM_NO_LIB_BUILD=
DO_KEYBOARDS_DOWNLOAD=false

while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    -download-resources)
      DO_KEYBOARDS_DOWNLOAD=true
      ;;
    -h|-\?)
      display_usage
      ;;
    -debug)
      PARAM_DEBUG=-debug
      ;;
    -no-daemon)
      PARAM_NO_DAEMON=-no-daemon
      ;;
    -no-update)
      PARAM_NO_UDPATE=-no-update
      ;;
    -lib-build)
      PARAM_LIB_BUILD=-lib-build
      ;;
    -no-lib-build|-lib-nobuild)
      PARAM_NO_LIB_BUILD=-no-lib-build
      ;;
  esac
  shift
done

# Verify default keyboard package exists
if [[ ! -f "$KEYBOARDS_TARGET" || ! -f "$KEYBOARDS_CSV_TARGET" ]]; then
  echo "$KEYBOARDS_TARGET and $KEYBOARDS_CSV_TARGET required. Will download the latest version"
  DO_KEYBOARDS_DOWNLOAD=true
fi

if [ ! -z "$PARAM_LIB_BUILD" ] && [ ! -z "$PARAM_NO_LIB_BUILD" ]; then
  echo "ERROR: Cannot set both -lib-build and -no-lib-build"
  exit 1
fi

# Download default keyboard package
if [ "$DO_KEYBOARDS_DOWNLOAD" = true ]; then
  echo "Copying keyboards.csv"
  cp "$KEYBOARDS_CSV" "$KEYBOARDS_CSV_TARGET"

  downloadKeyboardPackage "$KEYBOARD_PACKAGE_ID" "$KEYBOARDS_TARGET"
fi

# TODO: in the future build_common.sh should probably be shared with all oem products?
./build_common.sh $PARAM_DEBUG $PARAM_NO_DAEMON $PARAM_NO_UPDATE $PARAM_LIB_BUILD $PARAM_NO_LIB_BUILD
