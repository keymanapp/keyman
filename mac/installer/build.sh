#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/mac/mac.inc.sh"

builder_describe "Build Keyman for Mac installer package" clean configure build test
builder_parse "$@"

mac_verify_on_mac

### SET PATHS ###

INPUT_METHOD_DIR_NAME="Keyman4MacIM"
XCODE_WORKSPACE_EXT=".xcworkspace"
PRODUCT_NAME="Keyman"

KEYMAN_BUILD_PATH="${KEYMAN_ROOT}/mac/build"
INPUT_METHOD_BASE_PATH="${KEYMAN_ROOT}/mac/${INPUT_METHOD_DIR_NAME}"

KEYMAN_WORKSPACE_PATH="${KEYMAN_ROOT}/mac/${PRODUCT_NAME}${XCODE_WORKSPACE_EXT}"
INPUT_METHOD_WORKSPACE_PATH="${INPUT_METHOD_BASE_PATH}/${INPUT_METHOD_DIR_NAME}${XCODE_WORKSPACE_EXT}"

OUTPUT_DIRECTORY_PATH="${KEYMAN_BUILD_PATH}/upload/${KEYMAN_VERSION}"
PACKAGE_NAME="Keyman-${KEYMAN_VERSION_FOR_FILENAME}.pkg"
OUTPUT_PACKAGE_PATH="${OUTPUT_DIRECTORY_PATH}/${PACKAGE_NAME}"

# bundle IDs for installer packages
INSTALL_INPUT_METHOD_BUNDLE_ID="com.keyman.im.installer"
INSTALL_CONFIG_BUNDLE_ID="com.keyman.config.installer"

# ---------------------

function do_build() {
  archive_apps
  export_apps
  create_packages
  combine_packages
  builder_echo "build and package of Keyman installer complete"
}

function do_clean() {
  builder_echo "cleaning Keyman installer..."
  rm -rf "${KEYMAN_BUILD_PATH}/Config.xcarchive"
  rm -rf "${KEYMAN_BUILD_PATH}/Keyman.xcarchive"
  rm -rf "${KEYMAN_BUILD_PATH}/ConfigExport"
  rm -rf "${KEYMAN_BUILD_PATH}/KeymanExport"
  rm -rf "${KEYMAN_BUILD_PATH}/keyman-config.pkg"
  rm -rf "${KEYMAN_BUILD_PATH}/keyman-input-method.pkg"
  rm -rf "${KEYMAN_BUILD_PATH}/upload/"
}

function archive_apps() {
  # xcodebuild for x86_64 and arm64 (universal binary)
  mac_xcodebuild archive -workspace "$INPUT_METHOD_WORKSPACE_PATH" \
          -scheme Keyman \
          -configuration Release \
          -archivePath "${KEYMAN_BUILD_PATH}/Keyman.xcarchive" \
          ARCHS=\"arm64 x86_64\" \
          ONLY_ACTIVE_ARCH=NO \
          PRODUCT_VERSION=$KEYMAN_VERSION

  # xcodebuild for x86_64 and arm64 (universal binary)
  mac_xcodebuild archive -workspace "$KEYMAN_WORKSPACE_PATH" \
          -scheme Config \
          -configuration Release \
          -archivePath "${KEYMAN_BUILD_PATH}/Config.xcarchive" \
          ARCHS=\"arm64 x86_64\" \
          ONLY_ACTIVE_ARCH=NO \
          PRODUCT_VERSION=$KEYMAN_VERSION
}

function export_apps() {
  mac_xcodebuild -exportArchive -archivePath "${KEYMAN_BUILD_PATH}/Keyman.xcarchive" \
      -exportOptionsPlist ./ExportOptions.plist \
      -exportPath "${KEYMAN_BUILD_PATH}/KeymanExport"

  mac_xcodebuild -exportArchive -archivePath "${KEYMAN_BUILD_PATH}/Config.xcarchive" \
      -exportOptionsPlist ./ExportOptions.plist \
      -exportPath "${KEYMAN_BUILD_PATH}/ConfigExport"
}

function create_packages() {
  builder_echo "packaging Keyman input method"

  pkgbuild --component "${KEYMAN_BUILD_PATH}/KeymanExport/Keyman.app" \
      --install-location /tmp \
      --identifier "$INSTALL_INPUT_METHOD_BUNDLE_ID" \
      --scripts ./scripts \
      --version "$KEYMAN_VERSION" \
      "${KEYMAN_BUILD_PATH}/keyman-input-method.pkg"

  builder_echo "packaging Keyman Configuration app"
  pkgbuild --component "${KEYMAN_BUILD_PATH}/ConfigExport/Keyman Configuration.app" \
      --install-location /Applications \
      --identifier "$INSTALL_CONFIG_BUNDLE_ID" \
      --version "$KEYMAN_VERSION" \
      "${KEYMAN_BUILD_PATH}/keyman-config.pkg"
}

function combine_packages() {
  builder_echo "combining packages into product"

  mkdir -p "$OUTPUT_DIRECTORY_PATH"

  productbuild --package "${KEYMAN_BUILD_PATH}/keyman-input-method.pkg" \
      --package "${KEYMAN_BUILD_PATH}/keyman-config.pkg" \
      --version "$KEYMAN_VERSION" \
      --sign 5FCED4988F27D172C5628A16DBA4AE6CA0015D11 \
      "$OUTPUT_PACKAGE_PATH"

  builder_heading "Uploading install package to Apple for notarization"
  mac_notarize "$OUTPUT_DIRECTORY_PATH" "$OUTPUT_PACKAGE_PATH"

  builder_heading "Attempting to staple notarization to install package"
  xcrun stapler staple "$OUTPUT_PACKAGE_PATH" || builder_die "stapler failed"
}

builder_run_action clean do_clean
# builder_run_action configure
builder_run_action build do_build
# builder_run_action test
