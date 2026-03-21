#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder-basic.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/mac/mac.inc.sh"

mac_verify_on_mac

### SET PATHS ###

IM_NAME="Keyman4MacIM"
CONFIG_NAME="Config"
XCODE_PROJ_EXT=".xcodeproj"
XCODE_WORKSPACE_EXT=".xcworkspace"
PRODUCT_NAME="Keyman"

KEYMAN_MAC_BASE_PATH="${KEYMAN_ROOT}/mac"
KM4MIM_BASE_PATH="${KEYMAN_MAC_BASE_PATH}/${IM_NAME}"

KMIM_WORKSPACE_PATH="${KM4MIM_BASE_PATH}/${IM_NAME}${XCODE_WORKSPACE_EXT}"

KEYMAN_WORKSPACE_PATH="${KEYMAN_MAC_BASE_PATH}/${PRODUCT_NAME}${XCODE_WORKSPACE_EXT}"
#CONFIGAPP_BASE_PATH="${KEYMAN_MAC_BASE_PATH}/${CONFIGAPP_NAME}"
#CONFIGAPP_PROJ_PATH="${CONFIGAPP_BASE_PATH}/${CONFIGAPP_NAME}${XCODE_PROJ_EXT}"


# --- Configuration ---
#INPUT_METHOD_NAME="Keyman.app"
INSTALL_IM_BUNDLE_ID="com.keyman.im.installer"
INSTALL_CONFIG_BUNDLE_ID="com.keyman.config.installer"

OUTPUT_DIRECTORY_PATH="${KEYMAN_MAC_BASE_PATH}/output/${KEYMAN_VERSION}"
PACKAGE_NAME="Keyman-${KEYMAN_VERSION_FOR_FILENAME}.pkg"
OUTPUT_PACKAGE_PATH="${OUTPUT_DIRECTORY_PATH}/${PACKAGE_NAME}"


# ---------------------

# xcodebuild for x86_64 and arm64 (universal binary)
xcodebuild archive -workspace "$KMIM_WORKSPACE_PATH" \
        -scheme Keyman \
        -configuration Release \
        -archivePath ./build/Keyman.xcarchive \
        ARCHS="arm64 x86_64" \
        ONLY_ACTIVE_ARCH=NO

# xcodebuild for x86_64 and arm64 (universal binary)
xcodebuild archive -workspace "$KEYMAN_WORKSPACE_PATH" \
        -scheme Config \
        -configuration Release \
        -archivePath ./build/Config.xcarchive \
        ARCHS="arm64 x86_64" \
        ONLY_ACTIVE_ARCH=NO

# export
xcodebuild -exportArchive -archivePath ./build/Keyman.xcarchive \
    -exportOptionsPlist ./installer/ExportOptions.plist \
    -exportPath ./build/KeymanExport

xcodebuild -exportArchive -archivePath ./build/Config.xcarchive \
    -exportOptionsPlist ./installer/ExportOptions.plist \
    -exportPath ./build/ConfigExport

echo "packaging Keyman Configuration"

pkgbuild --component ./build/KeymanExport/Keyman.app \
    --install-location /tmp \
    --identifier "$INSTALL_IM_BUNDLE_ID" \
    --scripts ./installer/scripts \
    --version "$KEYMAN_VERSION" \
    ./build/keyman-input-method.pkg

pkgbuild --component ./build/ConfigExport/Config.app \
    --install-location /Applications \
    --identifier "$INSTALL_CONFIG_BUNDLE_ID" \
    --version "$KEYMAN_VERSION" \
    ./build/keyman-config.pkg

echo "packaging combo"

mkdir -p "$OUTPUT_DIRECTORY_PATH"

productbuild --package ./build/keyman-input-method.pkg \
    --package ./build/keyman-config.pkg \
    --sign "Developer ID Installer: Summer Institute of Linguistics, Inc (SIL) (3YE4W86L3G)" \
    "$OUTPUT_PACKAGE_PATH"

 #   ./build/KeymanExport/$PACKAGE_NAME

  builder_heading "Uploading install package to Apple for notarization"
  mac_notarize "$OUTPUT_DIRECTORY_PATH" "$OUTPUT_PACKAGE_PATH"

   builder_heading "Attempting to staple notarization to install package"
   xcrun stapler staple "$OUTPUT_PACKAGE_PATH" || builder_die "stapler failed"

  builder_heading "Attempting to staple notarization to install package"
  xcrun stapler staple "$OUTPUT_PACKAGE_PATH" || builder_die "stapler failed"

echo "build and package complete"


