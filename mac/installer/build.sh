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
PACKAGE_NAME="keyman-${KEYMAN_VERSION_FOR_FILENAME}.pkg"
OUTPUT_PACKAGE_PATH="${OUTPUT_DIRECTORY_PATH}/${PACKAGE_NAME}"

OUTPUT__DISTRIBUTION_XML="${KEYMAN_BUILD_PATH}/distribution.xml"

# bundle IDs for installer packages
INSTALL_INPUT_METHOD_BUNDLE_ID="com.keyman.im.installer"
INSTALL_CONFIG_BUNDLE_ID="com.keyman.config.installer"
INSTALL_KEYMAN_BUNDLE_ID="com.keyman.full.installer"

  # temporary staging root directory
PKG_STAGE="${KEYMAN_BUILD_PATH}/StagingRoot"

# ---------------------

function do_build() {
  archive_apps
  export_apps
  build_distribution_xml
  create_package_from_staging
  builder_echo "build and package of Keyman installer complete"
}

function do_clean() {
  builder_echo "cleaning Keyman installer..."
  rm -rf "${KEYMAN_BUILD_PATH}/Config.xcarchive"
  rm -rf "${KEYMAN_BUILD_PATH}/Keyman.xcarchive"
  rm -rf "${KEYMAN_BUILD_PATH}/ConfigExport"
  rm -rf "${KEYMAN_BUILD_PATH}/KeymanExport"
  rm -rf "${KEYMAN_BUILD_PATH}/keyman-component.pkg"
  rm -rf "${KEYMAN_BUILD_PATH}/upload/"
  rm -rf "$PKG_STAGE"
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
          SENTRY_ENVIRONMENT="$KEYMAN_VERSION_ENVIRONMENT" \
          TIER="$KEYMAN_TIER" \
          VERSION_TAG="$KEYMAN_VERSION_TAG" \
          VERSION_WITH_TAG="$KEYMAN_VERSION_WITH_TAG" \
          VERSION_GIT_TAG="$KEYMAN_VERSION_GIT_TAG" \
          VERSION_RELEASE="$KEYMAN_VERSION_RELEASE" \
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

function create_package_from_staging() {
  builder_echo "setup staging directory for Keyman package"
  
  rm -rf "$PKG_STAGE" # clear cache

  # Create the internal system paths
  mkdir -p "${PKG_STAGE}/Applications"
  mkdir -p "${PKG_STAGE}/Users/Shared/KeymanStaging"

  # copy application files to package staging
  cp -R "${KEYMAN_BUILD_PATH}/ConfigExport/Keyman Configuration.app" "${PKG_STAGE}/Applications/"
  cp -R "${KEYMAN_BUILD_PATH}/KeymanExport/Keyman.app" "${PKG_STAGE}/Users/Shared/KeymanStaging/"

  # copy LaunchAgent configuration file to package staging
  cp ./com.keyman.deferred.plist "${PKG_STAGE}/Users/Shared/KeymanStaging/"

  mkdir -p "$OUTPUT_DIRECTORY_PATH"

  builder_echo "calling pkgbuild"

  pkgbuild --root "$PKG_STAGE" \
      --component-plist ./"KeymanConfigComponent.plist" \
      --fakeroot \
      --scripts ./scripts \
      --install-location / \
      --identifier "$INSTALL_KEYMAN_BUNDLE_ID" \
      --version "$KEYMAN_VERSION" \
      "${KEYMAN_BUILD_PATH}/keyman-component.pkg"

  builder_echo "calling productbuild"

  productbuild --distribution "${KEYMAN_BUILD_PATH}/distribution.xml" \
      --package-path "${KEYMAN_BUILD_PATH}" \
      --resources ./installer-resources \
      --sign 5FCED4988F27D172C5628A16DBA4AE6CA0015D11 \
    "$OUTPUT_PACKAGE_PATH"

  builder_heading "Uploading install package to Apple for notarization"
  mac_notarize "$OUTPUT_DIRECTORY_PATH" "$OUTPUT_PACKAGE_PATH"

  builder_heading "Attempting to staple notarization to install package"
  xcrun stapler staple "$OUTPUT_PACKAGE_PATH" || builder_die "stapler failed"
}

function build_distribution_xml(){
cat <<EOF > "${OUTPUT__DISTRIBUTION_XML}"
<?xml version="1.0" encoding="utf-8"?>
<installer-gui-script minSpecVersion="2">
    <title>Keyman for macOS</title>
    <options customize="never" require-scripts="false" hostArchitectures="x86_64,arm64"/>
    <welcome file="welcome.rtf" />
    <license file="license.rtf" mime-type="text/rtf"/>
    <!-- Light Mode Background -->
    <background file="keyman-x20-y36-144.png" mime-type="image/png" alignment="center" scaling="none"/>
    <!-- Dark Mode Background -->
    <background-darkAqua file="keyman-x20-y36-144.png" mime-type="image/png" alignment="center" scaling="none"/>

    <conclusion file="conclusion.rtf" mime-type="text/rtf"/>

    <!-- Restrict to macOS 13 Ventura or newer -->
    <allowed-os-versions>
        <os-version min="13.0"/>
    </allowed-os-versions>

    <choices-outline>
        <line choice="default"/>
    </choices-outline>
    
    <choice id="default" visible="false" title="Keyman Core">
        <pkg-ref id="com.keyman.full.installer"/>
    </choice>

    <pkg-ref id="com.keyman.full.installer" version="$KEYMAN_VERSION" onConclusion="none">keyman-component.pkg</pkg-ref>
</installer-gui-script>
EOF

echo "Created distribution.xml at ${OUTPUT__DISTRIBUTION_XML}"
}

builder_run_action clean do_clean
# builder_run_action configure
builder_run_action build do_build
# builder_run_action test
