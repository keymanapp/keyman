#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# Include our resource functions; they're pretty useful!
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/build-help.inc.sh"

# Please note that this build script (understandably) assumes that it is running on Mac OS X.

builder_describe "Builds Keyman Engine and the Keyman app for use on iOS devices - iPhone and iPad." \
  "@/ios/engine    build" \
  "clean" \
  "configure" \
  "build" \
  "--sim-artifact  Also outputs a simulator-friendly test artifact corresponding to the build"

builder_parse "$@"

CONFIG="Release"
if builder_is_debug_build; then
  CONFIG="Debug"
fi

builder_describe_outputs \
  build     /ios/build/Build/Products/${CONFIG}-iphoneos/Keyman.xcarchive

# Base definitions (must be before do_clean call)
DERIVED_DATA="$KEYMAN_ROOT/ios/build"
BUILD_PATH="$DERIVED_DATA/Build/Products"

# Needed before `configure` action
DEFAULT_KBD_ID="sil_euro_latin"
DEFAULT_LM_ID="nrc.en.mtnt"

# Build product paths
APP_BUNDLE_PATH="$BUILD_PATH/${CONFIG}-iphoneos/Keyman.app"
KEYBOARD_BUNDLE_PATH="$BUILD_PATH/${CONFIG}-iphoneos/SWKeyboard.appex"
ARCHIVE_PATH="$BUILD_PATH/${CONFIG}-iphoneos/Keyman.xcarchive"

# Engine library build path
KEYMAN_XCFRAMEWORK="$BUILD_PATH/$CONFIG/KeymanEngine.xcframework"

XCODEFLAGS="-quiet -configuration $CONFIG"
XCODEFLAGS_EXT="$XCODEFLAGS -derivedDataPath \"$DERIVED_DATA\" -workspace ../keymanios.xcworkspace"

CODE_SIGN=
if builder_is_debug_build; then
  CODE_SIGN="CODE_SIGN_IDENTITY= CODE_SIGNING_REQUIRED=NO ${DEV_TEAM:-} CODE_SIGN_ENTITLEMENTS= CODE_SIGNING_ALLOWED=NO"
fi

### START OF THE BUILD ###

function do_clean () {
  rm -rf "$BUILD_PATH"
}

function do_configure() {
  # do literally nothing
  true
}

function build_app() {
  echo "Building offline help."
  build_help_html ios keyman/Keyman/Keyman/resources/OfflineHelp.bundle/Contents/Resources

  echo ""
  echo "Building Keyman app."

  # Provides a needed link for codesigning for our CI.
  if ! [ -z "${DEVELOPMENT_TEAM+x}" ]; then
    DEV_TEAM="DEVELOPMENT_TEAM=${DEVELOPMENT_TEAM}"
  else
    DEV_TEAM=
  fi

  # Time to prepare the deployment archive data.
  echo ""
  echo "Preparing .xcarchive for real devices."
  run_xcodebuild $XCODEFLAGS_EXT \
              $CODE_SIGN \
              -scheme Keyman \
              -archivePath "$ARCHIVE_PATH" \
              archive \
              -allowProvisioningUpdates \
              VERSION=$VERSION \
              VERSION_WITH_TAG=$VERSION_WITH_TAG \
              VERSION_ENVIRONMENT=$VERSION_ENVIRONMENT \
              UPLOAD_SENTRY=$UPLOAD_SENTRY

  assertDirExists "$ARCHIVE_PATH"

  if ! builder_is_debug_build; then
    echo "Preparing .ipa file for deployment to real devices"
    # Do NOT use the _EXT variant here; there's no scheme to ref, which will lead
    # Xcode to generate a build error.
    run_xcodebuild $XCODEFLAGS \
                -exportArchive \
                -archivePath "$ARCHIVE_PATH" \
                -exportOptionsPlist ../exportAppStore.plist \
                -exportPath "$BUILD_PATH/${CONFIG}-iphoneos" \
                -allowProvisioningUpdates
  fi

  if builder_has_option --sim-artifact; then
    echo "Preparing .app file for simulator-targeted artifact for testing"
    run_xcodebuild $XCODEFLAGS_EXT \
                $CODE_SIGN \
                -scheme Keyman \
                -sdk iphonesimulator \
                VERSION=$VERSION \
                VERSION_WITH_TAG=$VERSION_WITH_TAG \
                VERSION_ENVIRONMENT=$VERSION_ENVIRONMENT \
                UPLOAD_SENTRY=$UPLOAD_SENTRY
  fi
}

builder_run_action clean         do_clean
builder_run_action configure     do_configure
builder_run_action build         build_app