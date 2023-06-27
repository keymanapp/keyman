#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*/*}/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# TODO:  resolve how this is sourced by scripts in subfolders;
# we want to cd to the subfolder including this script.
#
# # This script runs from its own folder
# cd "$(dirname "$THIS_SCRIPT")"

# Include our resource functions; they're pretty useful!
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# Please note that this build script (understandably) assumes that it is running on Mac OS X.
verify_on_mac

if [ -z "$TARGET" ]; then
  exit 1
fi

builder_describe "Builds sample app $TARGET that demos the Keyman Engine for iPhone and iPad" \
  "@/ios/engine build" \
  "clean" \
  "configure" \
  "build" \
  "--debug          Avoids codesigning and adds full sourcemaps for the embedded predictive-text engine"

builder_parse "$@"

CONFIG=Release
if builder_is_debug_build; then
  CONFIG="Debug"
fi

# TODO: should probably define this.
# builder_describe_outputs \
#   build     /ios/build/Build/Products/Release-iphoneos/Keyman.xcarchive

BUILD_FOLDER=build

do_clean ( ) {
  rm -rf $BUILD_FOLDER
}

### START OF THE BUILD ###

KEYMAN_ENGINE_FRAMEWORK_SRC="$KEYMAN_ROOT/ios/build/Build/Products/$CONFIG/KeymanEngine.xcframework"
KEYMAN_ENGINE_FRAMEWORK_DST=./

function do_build() {
  # Copy resources.
  cp -Rf "$KEYMAN_ENGINE_FRAMEWORK_SRC" "$KEYMAN_ENGINE_FRAMEWORK_DST"

  CODE_SIGN=
  if ! builder_has_option --debug; then
    CODE_SIGN=CODE_SIGN_IDENTITY="" CODE_SIGNING_REQUIRED=NO CODE_SIGNING_ALLOWED="NO" CODE_SIGNING_ENTITLEMENTS=""
  fi

  run_xcodebuild -quiet $CODE_SIGN -target "$TARGET" -config "$CONFIG"
}

builder_run_action clean     do_clean
builder_run_action build     do_build