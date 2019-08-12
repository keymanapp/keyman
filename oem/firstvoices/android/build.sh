#!/bin/bash

# Set sensible script defaults:
# set -e: Terminate script if a command returns an error
set -e
# set -u: Terminate script if an unset variable is used
set -u
# set -x: Debugging use, print each statement
# set -x

display_usage ( ) {
  echo "build.sh [-no-daemon] [-debug] [-no-update] [-lib-build|-no-lib-build] [-copy-keyboards] [-clean-keyboards] [-h|-?]"
  echo "Build $TARGET"
  echo "  -no-daemon              Don't start the Gradle daemon. Use for CI"
  echo "  -debug                  Compile only Debug variant"
  echo "  -no-update              Don't copy or build the Keyman Engine library in (assumes already present)"
  echo "  -lib-build              Force rebuild of the Keyman Engine library"
  echo "  -no-lib-build           Only rebuild the Keyman Engine library if it doesn't exist in /android"
  echo "  -copy-keyboards         Only copy the keyboards; don't rebuild them"
  echo "  -clean-keyboards        Clean the keyboards from this repo"
  echo ""
  echo "This build script assumes that the https://github.com/keymanapp/keyboards repo is in the same parent"
  echo "folder as this repo, with the default name 'keyboards'"
  exit 1
}

export TARGET=FirstVoices
export KEYBOARDS_TARGET=app/src/main/assets/packages
export KEYBOARDS_CSV_TARGET=app/src/main/assets/keyboards.csv

# This build script assumes that the https://github.com/keymanapp/keyboards repo is in
# the same parent folder as this repo, with the default name 'keyboards'

export KEYBOARDS_ROOT=../../../../keyboards

PARAM_COPY_KEYBOARDS=
PARAM_CLEAN_KEYBOARDS=
PARAM_DEBUG=
PARAM_NO_DAEMON=
PARAM_NO_UPDATE=
PARAM_LIB_BUILD=
PARAM_NO_LIB_BUILD=

while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    -copy-keyboards)
      PARAM_COPY_KEYBOARDS=-copy-keyboards
      ;;
    -h|-?)
      display_usage
      ;;
    -clean-keyboards)
      PARAMS_CLEAN_KEYBOARDS=-clean-keyboards
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

if [ ! -z "$PARAM_LIB_BUILD" ] && [ ! -z "$PARAM_NO_LIB_BUILD" ]; then
  echo "ERROR: Cannot set both -lib-build and -no-lib-build"
  exit 1
fi

../common/build_keyboards.sh $PARAM_COPY_KEYBOARDS $PARAM_CLEAN_KEYBOARDS $PARAM_DEBUG

# TODO: in the future build_common.sh should probably be shared with all oem products?
./build_common.sh $PARAM_DEBUG $PARAM_NO_DAEMON $PARAM_NO_UPDATE $PARAM_LIB_BUILD $PARAM_NO_LIB_BUILD
