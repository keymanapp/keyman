#!/usr/bin/env bash

set -e
set -u

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

SCHEME_NAME="KeymanEngine"
FRAMEWORK_NAME="KeymanEngine"

# CONFIGURATION: one of [Debug, Debug + Sentry, Release]
#                - is auto-set by Xcode, is also managed by initial build script
XCFRAMEWORK="${BUILD_DIR}/${CONFIGURATION}/${FRAMEWORK_NAME}.xcframework"

IPHONE_FRAMEWORK="${BUILD_DIR}/${CONFIGURATION}-iphoneos/${FRAMEWORK_NAME}.framework"
SIMULATOR_FRAMEWORK="${BUILD_DIR}/${CONFIGURATION}-iphonesimulator/${FRAMEWORK_NAME}.framework"

echo ""
echo "Running: ${KEYMAN_ROOT}/ios/scripts/kme-universal.sh"
echo ""

# Used to build the appropriate archive files needed to construct the XCFramework.
# $1 - defines the target device type (`-destination` parameter for `xcodebuild`).
build_archive ( ) {
  # Note:  while official docs say we should use `xcodebuild archive ...`, that
  #        is not only markedly slower, it also adds undesired side-effects to
  #        our build processes.
  #        (It nukes the base .framework file used to build the archive with a
  #        broken alias that subsequent builds [like the main app's!] can't process.)
  run_xcodebuild build \
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
# -allow-internal-distribution:  preserves the .swiftmodule files, greatly
#                                simplifying integration in consuming apps.
#                                - Carthage uses this for its XCFramework support.
run_xcodebuild -create-xcframework \
  -allow-internal-distribution \
  -framework ${IPHONE_FRAMEWORK} \
  -framework ${SIMULATOR_FRAMEWORK} \
  -output ${XCFRAMEWORK}