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

IM_NAME="Keyman4MacIM"
CONFIGAPP_NAME="Keyman Configuration"
XCODE_PROJ_EXT=".xcodeproj"
XCODE_WORKSPACE_EXT=".xcworkspace"
PRODUCT_NAME="Keyman"

KEYMAN_MAC_BASE_PATH="${KEYMAN_ROOT}/mac"
KM4MIM_BASE_PATH="${KEYMAN_MAC_BASE_PATH}/${IM_NAME}"

KMIM_WORKSPACE_PATH="${KM4MIM_BASE_PATH}/${IM_NAME}${XCODE_WORKSPACE_EXT}"
KEYMAN_WORKSPACE_PATH="${KEYMAN_MAC_BASE_PATH}/${PRODUCT_NAME}${XCODE_WORKSPACE_EXT}"

# --- Configuration ---
#INPUT_METHOD_NAME="Keyman.app"
INSTALL_IM_BUNDLE_ID="com.keyman.im.installer"
INSTALL_CONFIG_BUNDLE_ID="com.keyman.config.installer"

# publish to path similar to that for .dmg so teamcity will show in artifacts
#OUTPUT_DIRECTORY_PATH="${KEYMAN_MAC_BASE_PATH}/output/upload/${KEYMAN_VERSION}"
OUTPUT_DIRECTORY_PATH="${KM4MIM_BASE_PATH}/output/upload/${KEYMAN_VERSION}"
PACKAGE_NAME="Keyman-${KEYMAN_VERSION_FOR_FILENAME}.pkg"
OUTPUT_PACKAGE_PATH="${OUTPUT_DIRECTORY_PATH}/${PACKAGE_NAME}"

# ---------------------

function do_build() {
  archive_apps
  export_apps
  create_packages
  combine_packages
  builder_echo "build and package of Keyman installer complete"
}

function archive_apps() {
  # xcodebuild for x86_64 and arm64 (universal binary)
  mac_xcodebuild archive -workspace "$KMIM_WORKSPACE_PATH" \
          -scheme Keyman \
          -configuration Release \
          -archivePath ../build/Keyman.xcarchive \
          ARCHS=\"arm64 x86_64\" \
          ONLY_ACTIVE_ARCH=NO

  # xcodebuild for x86_64 and arm64 (universal binary)
  mac_xcodebuild archive -workspace "$KEYMAN_WORKSPACE_PATH" \
          -scheme Config \
          -configuration Release \
          -archivePath ../build/Config.xcarchive \
          ARCHS=\"arm64 x86_64\" \
          ONLY_ACTIVE_ARCH=NO
}

function export_apps() {
  mac_xcodebuild -exportArchive -archivePath ../build/Keyman.xcarchive \
      -exportOptionsPlist ./ExportOptions.plist \
      -exportPath ../build/KeymanExport

  mac_xcodebuild -exportArchive -archivePath ../build/Config.xcarchive \
      -exportOptionsPlist ./ExportOptions.plist \
      -exportPath ../build/ConfigExport
}

function create_packages() {
  builder_echo "packaging Keyman input method"

  pkgbuild --component ../build/KeymanExport/Keyman.app \
      --install-location /tmp \
      --identifier "$INSTALL_IM_BUNDLE_ID" \
      --scripts ./scripts \
      --version "$KEYMAN_VERSION" \
      ../build/keyman-input-method.pkg

  builder_echo "packaging Keyman Configuration app"
  pkgbuild --component "../build/ConfigExport/$CONFIGAPP_NAME.app" \
      --install-location /Applications \
      --identifier "$INSTALL_CONFIG_BUNDLE_ID" \
      --version "$KEYMAN_VERSION" \
      ../build/keyman-config.pkg
}

function combine_packages() {
  builder_echo "combining packages into product"

  mkdir -p "$OUTPUT_DIRECTORY_PATH"

  productbuild --package ../build/keyman-input-method.pkg \
      --package ../build/keyman-config.pkg \
      --version "$KEYMAN_VERSION" \
      --sign 5FCED4988F27D172C5628A16DBA4AE6CA0015D11 \
      "$OUTPUT_PACKAGE_PATH"

  builder_heading "Uploading install package to Apple for notarization"
  mac_notarize "$OUTPUT_DIRECTORY_PATH" "$OUTPUT_PACKAGE_PATH"

  builder_heading "Attempting to staple notarization to install package"
  xcrun stapler staple "$OUTPUT_PACKAGE_PATH" || builder_die "stapler failed"
}

# builder_run_action clean ...
# builder_run_action configure ...
builder_run_action build do_build
# builder_run_action test ...
