#!/bin/bash

# Build the keyboards referred to in ../keyboards.csv and copy them + metadata
# to the FirstVoices/Keyboards folder

# This build script assumes that the https://github.com/keymanapp/keyboards repo is in
# the same parent folder as this repo, with the default name 'keyboards'
# Alternatively, the keyboards can be downloaded from downloads.keyman.com
#
# keyboards.csv has columns Shortname,ID,Name,Region,OldVersion

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

. "$KEYMAN_ROOT/resources/build/jq.inc.sh"

# This build script assumes that the https://github.com/keymanapp/keyboards repo is in
# the same parent folder as this repo, with the default name 'keyboards'
KEYBOARDS_ROOT="$KEYMAN_ROOT/../keyboards"

function die {
  echo "FATAL: $1"
  exit 99
}

function display_usage {
  echo "Usage: build_keyboards.sh [-download-keyboards] [-copy-keyboards] [-clean-keyboards] [-debug] [-h|-?]"
  echo "Builds all keyboards used by the app and copies them into the"
  echo "target path."
  echo "  -download-keyboards: Download fv_all.kmp from downloads.keyman.com"
  echo "  -copy-keyboards: Only copy the keyboards; don't rebuild them"
  echo "  -clean-keyboards: Clean the keyboards from this repo"
  echo "  -debug: Build debug versions of the keyboards"
  exit 1
}

DO_CLEAN=true
DO_COPY=true
DO_BUILD=true
DO_DOWNLOAD=false

while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    -download-keyboards)
      DO_DOWNLOAD=true
      DO_COPY=false
      DO_BUILD=false
      ;;
    -copy-keyboards)
      DO_BUILD=false
      ;;
    -h|-\?)
      display_usage
      ;;
    -clean-keyboards)
      DO_COPY=false
      DO_BUILD=false
      ;;
    -debug)
      BUILD_FLAGS="-debug"
      ;;
  esac
  shift
done

# Check that $KEYBOARDS_TARGET is valid
if [[ -z "$KEYBOARDS_TARGET" ]]; then
  die "KEYBOARDS_TARGET cannot be empty"
fi

# Clean existing Keyboards folder
# Can't remove entire folder because Android has other assets/

if [ $DO_CLEAN = true ]; then
  echo "Cleaning target path $KEYBOARDS_TARGET"
  rm -f "$KEYBOARDS_TARGET/*.kmp"
  rm -f "$KEYBOARDS_TARGET/*.keyboard_info"
  echo "Removing file $KEYBOARDS_CSV_TARGET"
  rm -f $KEYBOARDS_CSV_TARGET
fi

mkdir -p $KEYBOARDS_TARGET

# Build keyboards and copy into KEYBOARDS_TARGET

if [ $DO_BUILD = true ] || [ $DO_COPY = true ]; then
  echo "Building and/or copying keyboards"

  if [ $DO_COPY = true ]; then
    cp ../keyboards.csv "$KEYBOARDS_CSV_TARGET"
  fi

  SCRIPT_ROOT=`pwd`
  pushd $KEYBOARDS_ROOT

  {
    # Skip header line
    read

    # Read CSV and build each referenced keyboard
    while IFS=, read -r shortname id name region old_keyboard; do
      if [ $DO_BUILD = true ]; then
        echo "Building $id ($name)" # $shortname/$id -> $name"
        WINEDEBUG=fixme-nls,fixme-thread "./build.sh" release/$shortname/$id || die "Unable to build keyboard $shortname/$id"
      fi
      if [ $DO_COPY = true ]; then
        echo "Copying $id ($name) to $KEYBOARDS_TARGET"
        mkdir -p "$SCRIPT_ROOT/$KEYBOARDS_TARGET/$id"
        unzip -o release/$shortname/$id/build/$id.kmp $id.js kmp.json -d "$SCRIPT_ROOT/$KEYBOARDS_TARGET/$id/"
        cp release/$shortname/$id/build/$id.keyboard_info "$SCRIPT_ROOT/$KEYBOARDS_TARGET/$id.keyboard_info"
      fi
#        die "done"
    done
  } < "$SCRIPT_ROOT/../keyboards.csv"

  popd
fi

if [ $DO_DOWNLOAD = true ]; then
  echo "Downloading keyboards from downloads.keyman.com"

  cp ../keyboards.csv "$KEYBOARDS_CSV_TARGET"

  SCRIPT_ROOT=`pwd`
  URL_DOWNLOAD=https://downloads.keyman.com
  URL_API_VERSION=${URL_DOWNLOAD}/api/keyboard/

  id="fv_all"
  echo "Downloading $id"
  URL_DOWNLOAD_FILE=`curl -s "$URL_API_VERSION/$id" | "$JQ" -r .kmp`
  curl -s "$URL_DOWNLOAD_FILE" > "$SCRIPT_ROOT/$KEYBOARDS_TARGET/$id.kmp"
fi

echo "Keyboards built successfully."
