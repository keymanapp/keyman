#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/mac/mac.inc.sh"
. "$KEYMAN_ROOT/resources/build/build-download-resources.sh"

builder_describe "Builds Keyman Engine for use on iOS devices - iPhone and iPad." \
  "@/web/src/app/webview        build" \
  "@/web/src/engine/sentry-manager  build" \
  "clean" \
  "configure" \
  "build" \
  "--sim-artifact  Also outputs a simulator-friendly test artifact corresponding to the build"

builder_parse "$@"

mac_verify_on_mac

CONFIG="Release"
if builder_is_debug_build; then
  CONFIG="Debug"
fi

builder_describe_outputs \
  build         /ios/build/Build/Products/Release/KeymanEngine.xcframework

# Base definitions (must be before do_clean call)
DERIVED_DATA="$KEYMAN_ROOT/ios/build"
BUILD_PATH="$DERIVED_DATA/Build/Products"

# Extended path definitions
BUNDLE_PATH="${THIS_SCRIPT_PATH}/KMEI/KeymanEngine/resources/Keyman.bundle/Contents/Resources"

# Needed before `configure` action
DEFAULT_KBD_ID="sil_euro_latin"
DEFAULT_LM_ID="nrc.en.mtnt"

# Engine library build path
KEYMAN_XCFRAMEWORK="$BUILD_PATH/$CONFIG/KeymanEngine.xcframework"

XCODEFLAGS="-quiet -configuration $CONFIG"
XCODEFLAGS_EXT="$XCODEFLAGS -derivedDataPath \"$DERIVED_DATA\" -workspace ../keymanios.xcworkspace"

CODE_SIGN=
if builder_is_debug_build || ( builder_is_ci_build && ! builder_is_ci_build_level_release ); then
  CODE_SIGN="CODE_SIGN_IDENTITY= CODE_SIGNING_REQUIRED=NO ${DEV_TEAM:-} CODE_SIGN_ENTITLEMENTS= CODE_SIGNING_ALLOWED=NO"
fi

### START OF THE BUILD ###

function do_clean () {
  # Possible TODO:  can we clean the engine target without also cleaning the app target?
  rm -rf "$BUILD_PATH"
}

function do_packages() {
  mkdir -p "$BUNDLE_PATH"

  downloadKeyboardPackage "$DEFAULT_KBD_ID"  "$BUNDLE_PATH/$DEFAULT_KBD_ID.kmp"
  downloadModelPackage    "$DEFAULT_LM_ID"   "$BUNDLE_PATH/$DEFAULT_LM_ID.model.kmp"
}

function do_configure ( ) {
  do_packages
}

# Manages KeymanEngine.bundle, which is included inside the :engine target.
function update_bundle ( ) {
  mkdir -p "$BUNDLE_PATH"

  KMW_PRODUCT="$KEYMAN_ROOT/web/build/app/webview/$CONFIG"
  KMW_RESOURCES="$KEYMAN_ROOT/web/build/app/resources"

  #Copy over the relevant resources!  It's easiest to do if we navigate to the resulting folder.
  cp "$KMW_RESOURCES/osk/kmwosk.css"            "$BUNDLE_PATH/kmwosk.css"
  cp "$KMW_RESOURCES/osk/keymanweb-osk.ttf"     "$BUNDLE_PATH/keymanweb-osk.ttf"
  cp "$KMW_PRODUCT/keymanweb-webview.js"        "$BUNDLE_PATH/keymanweb-webview.js"
  cp "$KEYMAN_ROOT/web/src/engine/sentry-manager/build/lib/index.js"     "$BUNDLE_PATH/keyman-sentry.js"
}

# First things first - update our dependencies.
function build_engine() {
  update_bundle

  echo
  echo "Build products will be set with the following version metadata:"
  echo "  * KEYMAN_VERSION=$KEYMAN_VERSION"
  echo "  * KEYMAN_VERSION_WITH_TAG=$KEYMAN_VERSION_WITH_TAG"
  echo "  * KEYMAN_VERSION_ENVIRONMENT=$KEYMAN_VERSION_ENVIRONMENT"
  echo "  * UPLOAD_SENTRY=$UPLOAD_SENTRY"
  echo
  echo "Building KMEI..."

  rm -rf "$BUILD_PATH/$CONFIG-universal"

  mac_xcodebuild $XCODEFLAGS_EXT \
            $CODE_SIGN \
            -scheme KME-universal \
            KEYMAN_VERSION=$KEYMAN_VERSION \
            KEYMAN_VERSION_WITH_TAG=$KEYMAN_VERSION_WITH_TAG \
            KEYMAN_VERSION_ENVIRONMENT=$KEYMAN_VERSION_ENVIRONMENT \
            UPLOAD_SENTRY=$UPLOAD_SENTRY

  if ! [[ -d "${KEYMAN_XCFRAMEWORK}" ]]; then
    builder_die "Build failed: directory '${KEYMAN_XCFRAMEWORK}' missing"
  fi
}

builder_run_action clean         do_clean
builder_run_action configure     do_configure
builder_run_action build         build_engine
