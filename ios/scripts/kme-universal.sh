#!/bin/sh

# Enabling the following option will cause build failures due to issues
# with difficult-to-resolve XIB errors in compilation.
#set -e
set -u

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

SCHEME_NAME="KeymanEngine"
FRAMEWORK_NAME="KeymanEngine"

XCFRAMEWORK="${BUILD_DIR}/${CONFIGURATION}/${FRAMEWORK_NAME}.xcframework"

IPHONE_ARCHIVE="${BUILD_DIR}/${CONFIGURATION}-iphoneos/${FRAMEWORK_NAME}.xcarchive"
SIMULATOR_ARCHIVE="${BUILD_DIR}/${CONFIGURATION}-iphonesimulator/${FRAMEWORK_NAME}.xcarchive"

IPHONE_FRAMEWORK="${BUILD_DIR}/${CONFIGURATION}-iphoneos/${FRAMEWORK_NAME}.framework"
SIMULATOR_FRAMEWORK="${BUILD_DIR}/${CONFIGURATION}-iphonesimulator/${FRAMEWORK_NAME}.framework"

echo ""
echo "Running: ${KEYMAN_ROOT}/ios/scripts/kme-universal.sh"
echo ""

# Used to build the appropriate archive files needed to construct the XCFramework.
# $1 - defines the target device type (`-destination` parameter for `xcodebuild`).
build_archive ( ) {
  xcodebuild build \
             -scheme "${SCHEME_NAME}" \
             -configuration ${CONFIGURATION} \
             -sdk "$1" \
             -quiet \
             BUILD_DIR="${BUILD_DIR}" \
             BUILD_ROOT="${BUILD_ROOT}" \
             VERSION=$VERSION \
             VERSION_WITH_TAG=$VERSION_WITH_TAG \
             VERSION_ENVIRONMENT=$VERSION_ENVIRONMENT \
             UPLOAD_SENTRY=$UPLOAD_SENTRY
}

# Build the target-specfic archives
echo ""
echo "Building iphoneos archive"
echo ""
build_archive iphoneos

echo ""
echo "Building iphonesimulator archive"
echo ""
build_archive iphonesimulator

# Clean old build product if it exists
if [ -e ${XCFRAMEWORK} ]; then
  rm -rf "${XCFRAMEWORK}"
fi

echo ""
echo "Compiling ${FRAMEWORK_NAME}.xcframework"
echo ""

# Build the final XCFramework file.
xcodebuild -create-xcframework \
  -framework ${IPHONE_FRAMEWORK} \
  -framework ${SIMULATOR_FRAMEWORK} \
  -output ${XCFRAMEWORK}