#!/bin/sh

# Build the keyboards referred to in ../keyboards.csv and copy them + metadata 
# to the FirstVoices/Keyboards folder

# keyboards.csv has columns Shortname,ID,Name,Region,OldVersion

# Set sensible script defaults:
# set -e: Terminate script if a command returns an error
set -e
# set -u: Terminate script if an unset variable is used
set -u
# set -x: Debugging use, print each statement
# set -x

# This build script assumes that the https://github.com/keymanapp/keyboards repo is in
# the same parent folder as this repo, with the default name 'keyboards'
KEYBOARDS_ROOT=../../../../keyboards
TARGET=FirstVoices/Keyboards



function die {
  echo "FATAL: $1"
  exit 99
}

function display_usage {
  echo "Usage: build_keyboards.sh [-copy-keyboards] [-clean-keyboards] [-debug] [-h|-?]"
  echo "Builds all keyboards used by the app and copies them into the"
  echo "target path."
  echo "  -copy-keyboards: Only copy the keyboards; don't rebuild them"
  echo "  -clean-keyboards: Clean the keyboards from this repo"
  echo "  -debug: Build debug versions of the keyboards"
  exit 1
}

DO_CLEAN=true
DO_COPY=true
DO_BUILD=true

while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    -copy-keyboards)
      DO_BUILD=false
      ;;
    -h|-?)
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

# Clean existing Keyboards folder

if [ $DO_CLEAN = true ]; then
  echo "Cleaning target path $TARGET"
  rm -f $TARGET/files/*
  rm -f $TARGET/keyboards.csv
fi

mkdir -p $TARGET/files

# Build keyboards and copy into ios/FirstVoices/Keyboards/files

if [ $DO_BUILD = true ] || [ $DO_COPY = true ]; then
  echo "Building and/or copying keyboards"

  if [ $DO_COPY = true ]; then
    cp ../keyboards.csv "$TARGET/keyboards.csv"
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
        echo "Copying $id ($name) to $TARGET"
        cp release/$shortname/$id/build/$id.js "$SCRIPT_ROOT/$TARGET/files/$id.js"
        cp release/$shortname/$id/build/$id.keyboard_info "$SCRIPT_ROOT/$TARGET/files/$id.keyboard_info"
      fi
#        die "done"
    done
  } < "$SCRIPT_ROOT/../keyboards.csv"

  popd
fi
