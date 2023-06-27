#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*/*}/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# This script runs from its own folder
cd "$(dirname "$THIS_SCRIPT")"

# Include our resource functions; they're pretty useful!
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/build-download-resources.sh"

# Please note that this build script (understandably) assumes that it is running on Mac OS X.
verify_on_mac

builder_describe "Builds Keyman Engine for use on iOS devices - iPhone and iPad." \
  "@/web/src/app/webview        build" \
  "@/common/web/sentry-manager  build" \
  "clean" \
  "configure" \
  "build" \
  "--debug         Avoids codesigning and adds full sourcemaps for the embedded predictive-text engine" \
  "--sim-artifact  Also outputs a simulator-friendly test artifact corresponding to the build"

builder_parse "$@"

CONFIG="release"
if builder_is_debug_build; then
  CONFIG="debug"
fi

builder_describe_outputs \
  configure     /ios/Carthage/Build     \
  build         /ios/build/Build/Products/Release/KeymanEngine.xcframework

# Base definitions (must be before do_clean call)
DERIVED_DATA=build
BUILD_PATH=$DERIVED_DATA/Build/Products

# Extended path definitions
KMEI_RESOURCES=engine/KMEI/KeymanEngine/resources
BUNDLE_PATH=$KMEI_RESOURCES/Keyman.bundle/Contents/Resources
KMW_ROOT=../web

# Needed before `configure` action
DEFAULT_KBD_ID="sil_euro_latin"
DEFAULT_LM_ID="nrc.en.mtnt"

# Engine library build path
KEYMAN_XCFRAMEWORK=$BUILD_PATH/$CONFIG/KeymanEngine.xcframework

XCODEFLAGS="-quiet -configuration $CONFIG"
XCODEFLAGS_EXT="$XCODEFLAGS -derivedDataPath $DERIVED_DATA -workspace ../keymanios.xcworkspace"

CODE_SIGN=
if builder_has_option --debug; then
  CODE_SIGN="CODE_SIGN_IDENTITY= CODE_SIGNING_REQUIRED=NO ${DEV_TEAM:-} CODE_SIGN_ENTITLEMENTS= CODE_SIGNING_ALLOWED=NO"
fi

### START OF THE BUILD ###

function do_clean () {
  # Possible TODO:  can we clean the engine target without also cleaning the app target?
  rm -rf $BUILD_PATH
  rm -rf Carthage
}

function carthage_die() {
  local msg="$1"

  # Don't leave a trace of the failed folder; we'd have to rebuild stuff anyway.
  # This way, a later re-run doesn't think `configure` succeeded when it did not.
  rm -rf Carthage
  builder_die "$1"
}

function do_carthage() {
  # Carthage setup must be handled from the directory with the Cartfile / Cartfile.resolved, it seems.
  pushd .. > /dev/null

  carthage checkout || carthage_die "Carthage dependency checkout failed"

  # --no-use-binaries: due to https://github.com/Carthage/Carthage/issues/3134,
  # which affects the sentry-cocoa dependency.
  carthage build --use-xcframeworks --no-use-binaries --platform iOS || carthage_die "Carthage dependency loading failed"

  popd > /dev/null
}

function do_packages() {
  if ! [ -d "$BUNDLE_PATH" ]; then
      mkdir -p "$BUNDLE_PATH"
  fi

  local base_dir="$(pwd)"

  downloadKeyboardPackage "$DEFAULT_KBD_ID"  "$base_dir/$BUNDLE_PATH/$DEFAULT_KBD_ID.kmp"
  downloadModelPackage    "$DEFAULT_LM_ID"   "$base_dir/$BUNDLE_PATH/$DEFAULT_LM_ID.model.kmp"
}

function do_configure ( ) {
  do_packages
  do_carthage
}

# Manages KeymanEngine.bundle, which is included inside the :engine target.
function update_bundle ( ) {
  if ! [ -d "$BUNDLE_PATH" ]; then
      mkdir -p "$BUNDLE_PATH"
  fi

  local base_dir="$(pwd)"

  KMW_PRODUCT=web/build/app/webview/
  KMW_RESOURCES=web/build/app/resources
  if builder_has_option --debug; then
    KMW_PRODUCT="$KMW_PRODUCT/debug"
  else
    KMW_PRODUCT="$KMW_PRODUCT/release"
  fi

  #Copy over the relevant resources!  It's easiest to do if we navigate to the resulting folder.
  cp "$KEYMAN_ROOT/$KMW_RESOURCES/osk/kmwosk.css"            "$base_dir/$BUNDLE_PATH/kmwosk.css"
  cp "$KEYMAN_ROOT/$KMW_RESOURCES/osk/keymanweb-osk.ttf"     "$base_dir/$BUNDLE_PATH/keymanweb-osk.ttf"
  cp "$KEYMAN_ROOT/$KMW_PRODUCT/keymanweb-webview.js"        "$base_dir/$BUNDLE_PATH/keymanweb-webview.js"

  if builder_has_option --debug; then
    cp "$KEYMAN_ROOT/$KMW_PRODUCT/keymanweb-webview.js.map"  "$base_dir/$BUNDLE_PATH/keymanweb-webview.js.map"
  elif [ -f "$base_dir/$BUNDLE_PATH/keymanweb-webview.js.map" ]; then
    rm      "$base_dir/$BUNDLE_PATH/keymanweb-webview.js.map"
  fi

  cp "$KEYMAN_ROOT/node_modules/@sentry/browser/build/bundle.min.js" "$base_dir/$BUNDLE_PATH/sentry.min.js"
  cp "$KEYMAN_ROOT/common/web/sentry-manager/build/lib/index.js"   "$base_dir/$BUNDLE_PATH/keyman-sentry.js"
}

# First things first - update our dependencies.
function build_engine() {
  update_bundle

  echo
  echo "Build products will be set with the following version metadata:"
  echo "  * VERSION=$VERSION"
  echo "  * VERSION_WITH_TAG=$VERSION_WITH_TAG"
  echo "  * VERSION_ENVIRONMENT=$VERSION_ENVIRONMENT"
  echo "  * UPLOAD_SENTRY=$UPLOAD_SENTRY"
  echo
  echo "Building KMEI..."

  if [ -d "$BUILD_PATH/$CONFIG-universal" ]; then
    rm -r $BUILD_PATH/$CONFIG-universal
  fi

  run_xcodebuild $XCODEFLAGS_EXT $CODE_SIGN -scheme KME-universal \
            VERSION=$VERSION \
            VERSION_WITH_TAG=$VERSION_WITH_TAG \
            VERSION_ENVIRONMENT=$VERSION_ENVIRONMENT \
            UPLOAD_SENTRY=$UPLOAD_SENTRY

  assertDirExists "$KEYMAN_XCFRAMEWORK"
}

builder_run_action clean         do_clean
builder_run_action configure     do_configure
builder_run_action build         build_engine