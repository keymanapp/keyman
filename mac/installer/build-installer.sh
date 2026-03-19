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
PRODUCT_NAME="Keyman"

KEYMAN_MAC_BASE_PATH="$KEYMAN_ROOT/mac"
KM4MIM_BASE_PATH="$KEYMAN_MAC_BASE_PATH/$IM_NAME"
CONFIG_BASE_PATH="$KEYMAN_MAC_BASE_PATH/$CONFIG_NAME"

KMIM_WORKSPACE_PATH="$KM4MIM_BASE_PATH/$IM_NAME.xcworkspace"
CONFIG_PROJ_PATH="$CONFIG_BASE_PATH/$CONFIG_NAME$XCODE_PROJ_EXT"
KMIM_PROJ_PATH="$KM4MIM_BASE_PATH/$IM_NAME$XCODE_PROJ_EXT"


# --- Configuration ---
INPUT_METHOD_NAME="Keyman.app"
BUNDLE_IDENTIFIER="com.keyman.installer" # Replace with your organization identifier
VERSION="1.0"
INSTALL_LOCATION="/Library/Input Methods"
PKG_OUTPUT_NAME="Keyman-$VERSION.pkg"
# ---------------------

# xcodebuild for x86_64 and arm64 (universal binary)
xcodebuild archive -workspace \"$KMIM_WORKSPACE_PATH\"
        -scheme Keyman \
        -configuration Release \
        -archivePath ./build/Keyman.xcarchive \
        ARCHS="arm64 x86_64" \
        ONLY_ACTIVE_ARCH=NO

# export
xcodebuild -exportArchive -archivePath ./build/Keyman.xcarchive \
    -exportOptionsPlist ./installer/ExportOptions.plist \
    -exportPath ./build/KeymanExport

echo "packaging Keyman Configuration"

pkgbuild --component ./build/KeymanExport/Keyman\ Configuration.app \
    --install-location /Applications \
    --identifier $BUNDLE_IDENTIFIER \
    --version 1.1 \
    ./keyman-config.pkg

echo "packaging Keyman"

pkgbuild --component ./Resources/Keyman.app \
    --install-location /tmp \
    --identifier $BUNDLE_IDENTIFIER \
    --scripts ./Resources/Scripts \
    --version 1.1 \
    ./keyman-input-method.pkg

#pkgbuild --component /path/to/MyApp.app --install-location /Applications --sign "" /path/to/MyAppSigned.pkg

#pkgbuild --component ./build/KeymanExport/Keyman.app --install-location /Applications --sign "Developer ID Application: Summer Institute of Linguistics, Inc (SIL) (3YE4W86L3G)" ./Keyman.pkg
#pkgbuild --root payload --install-location "~/Library/Input Methods" --identifier com.keyputim.installer --version 1.0 KeymanIM.pkg

echo "packaging combo"

productbuild --package ./keyman-config.pkg --package ./keyman-input-method.pkg \
    --sign "Developer ID Installer: Summer Institute of Linguistics, Inc (SIL) (3YE4W86L3G)" \
    ./keyman.pkg

echo "build and package complete"


