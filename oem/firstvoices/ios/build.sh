#!/usr/bin/env bash
# Build FirstVoices for iOS app

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# Include our resource functions; they're pretty useful!
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/build-download-resources.sh"

# ################################ Main script ################################

# Please note that this build script (understandably) assumes that it is running on Mac OS X.
verify_on_mac

export TARGET=FirstVoices

builder_describe "Builds the $TARGET app for use on iOS devices - iPhone and iPad." \
  "@/ios:engine   build" \
  "clean" \
  "configure" \
  "build" \
  "--sim-artifact  Also outputs a simulator-friendly test artifact corresponding to the build"

builder_parse "$@"

KMEI_BUILD_DIR="$KEYMAN_ROOT/ios/build"
DERIVED_DATA="$THIS_SCRIPT_PATH/build"

do_clean ( ) {
  rm -rf "$DERIVED_DATA"
  rm -rf Carthage
}

### START OF THE BUILD ###

CONFIG=Release
if builder_is_debug_build; then
  CONFIG="Debug"
fi

KEYMAN_ENGINE_FRAMEWORK_SRC="$KMEI_BUILD_DIR/Build/Products/$CONFIG/KeymanEngine.xcframework"
KEYMAN_ENGINE_FRAMEWORK_DST=./

# First things first - update our dependencies.

function carthage_die() {
  local msg="$1"

  # Don't leave a trace of the failed folder; we'd have to rebuild stuff anyway.
  # This way, a later re-run doesn't think `configure` succeeded when it did not.
  rm -rf Carthage
  builder_die "$1"
}

function do_configure() {
  KEYBOARDS_CSV="$KEYMAN_ROOT/oem/firstvoices/keyboards.csv"
  KEYBOARDS_CSV_TARGET="$KEYMAN_ROOT/oem/firstvoices/ios/FirstVoices/Keyboards/keyboards.csv"

  KEYBOARD_PACKAGE_ID="fv_all"
  KEYBOARDS_TARGET="$KEYMAN_ROOT/oem/firstvoices/ios/FirstVoices/Keyboards/${KEYBOARD_PACKAGE_ID}.kmp"

  mkdir -p "$KEYMAN_ROOT/oem/firstvoices/ios/FirstVoices/Keyboards"
  cp "$KEYBOARDS_CSV" "$KEYBOARDS_CSV_TARGET"
  downloadKeyboardPackage "$KEYBOARD_PACKAGE_ID" "KEYBOARDS_TARGET"

  echo
  echo "Load dependencies with Carthage"

  carthage checkout || carthage_die "Carthage dependency checkout failed"

  # --no-use-binaries: due to https://github.com/Carthage/Carthage/issues/3134,
  # which affects the sentry-cocoa dependency.
  carthage build --use-xcframeworks --no-use-binaries --platform iOS || carthage_die "Carthage dependency loading failed"
}

#
# Build Target Application
#

BUILD_PATH="$DERIVED_DATA/Build/Products"
XCODEFLAGS="-quiet -configuration $CONFIG"
XCODEFLAGS_EXT="$XCODEFLAGS -derivedDataPath \"$DERIVED_DATA\"" # -workspace $TARGET.xcworkspace"
ARCHIVE_PATH="$BUILD_PATH/${CONFIG}-iphoneos/$TARGET.xcarchive"

CODE_SIGN=
if builder_is_debug_build; then
  CODE_SIGN="CODE_SIGN_IDENTITY= CODE_SIGNING_REQUIRED=NO ${DEV_TEAM:-} CODE_SIGN_ENTITLEMENTS= CODE_SIGNING_ALLOWED=NO"
fi

function do_build() {
  # Copy resources.
  cp -Rf "$KEYMAN_ENGINE_FRAMEWORK_SRC" "$KEYMAN_ENGINE_FRAMEWORK_DST"

  if ! builder_is_debug_build; then
    # Huh, this is inverted from the main Keyman build, which goes .xcarchive -> .ipa instead.

    # Time to prepare the deployment archive data.
    echo ""
    echo "Preparing .ipa file for deployment to real devices."
    run_xcodebuild $XCODEFLAGS_EXT \
            -scheme $TARGET \
            -archivePath "$ARCHIVE_PATH" \
            archive \
            -allowProvisioningUpdates \
            VERSION=$VERSION \
            VERSION_WITH_TAG=$VERSION_WITH_TAG

    assertDirExists "$ARCHIVE_PATH"


    # Do NOT use the _EXT variant here; there's no scheme to ref, which will lead
    # Xcode to generate a build error.
    run_xcodebuild $XCODEFLAGS \
            -exportArchive \
            -archivePath "$ARCHIVE_PATH" \
            -exportOptionsPlist exportAppStore.plist \
            -exportPath "$BUILD_PATH/${CONFIG}-iphoneos" \
            -allowProvisioningUpdates \
            VERSION=$VERSION \
            VERSION_WITH_TAG=$VERSION_WITH_TAG
  else
    run_xcodebuild $CODE_SIGN \
            $XCODEFLAGS_EXT \
            -scheme "$TARGET" \
            VERSION=$VERSION \
            VERSION_WITH_TAG=$VERSION_WITH_TAG
  fi

  if builder_has_option --sim-artifact; then
    echo "Preparing .app file as Simulator-targeted build artifact."
    run_xcodebuild $CODE_SIGN \
            $XCODEFLAGS_EXT \
            -scheme "$TARGET" \
            -sdk iphonesimulator \
            VERSION=$VERSION \
            VERSION_WITH_TAG=$VERSION_WITH_TAG
  fi
}

builder_run_action clean       do_clean
builder_run_action configure   do_configure
builder_run_action build       do_build
