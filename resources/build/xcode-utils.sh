#!/bin/bash

# This script will automatically have Xcode's build environment (and variables),
# so there's no need to do anything extra to fetch them.

# Takes one parameter:  "TAGGED".  Defaults to false (iOS: KMEI, SWKeyboard), sets tagged info if true (iOS: Keyman)
#
# Thanks to https://medium.com/@bestiosdevelope/automatic-build-incrementation-technique-for-ios-release-94eb0d08785b
# for its documentation in this matter.
function phaseSetBundleVersions() {
  if [[ $# -gt 0 ]]; then
    TAGGED=$1
  else
    TAGGED=false
  fi

  # For command-line builds, VERSION and VERSION_WITH_TAG) are forwarded through xcodebuild.
  if [ -z $VERSION ]; then
    # We're not a command-line build... so we'll need to retrieve these values ourselves with ./build-utils.sh.
    # Note that this script's process will not have access to TC environment variables, but that's fine for
    # local builds triggered through Xcode's UI, which aren't part of our CI processes.
    . $KEYMAN_ROOT/resources/build/build-utils.sh
    echo "UI build - fetching version from repository:"
    echo "  Plain:  $VERSION"
    echo "  Tagged: $VERSION_WITH_TAG"
  else
    echo "Command-line build - using provided version parameters"
  fi

  # Now, to set the version.
  APP_PLIST="$TARGET_BUILD_DIR/$INFOPLIST_PATH"
  echo "Setting $VERSION for $TARGET_BUILD_DIR/$INFOPLIST_PATH"
  /usr/libexec/Plistbuddy -c "Set :CFBundleVersion $VERSION" "$APP_PLIST"
  /usr/libexec/Plistbuddy -c "Set :CFBundleShortVersionString $VERSION" "$APP_PLIST"

  # Only attempt to write this when directly specified (otherwise, generates minor warning)
  if [ $TAGGED == true ]; then
  echo "Setting $VERSION_WITH_TAG for tagged version"
  /usr/libexec/Plistbuddy -c "Set :KeymanVersionWithTag $VERSION_WITH_TAG" "$APP_PLIST"
  fi

  if [ -f "${BUILT_PRODUCTS_DIR}/${WRAPPER_NAME}.dSYM/Contents/Info.plist" ]; then
    DSYM_PLIST="${BUILT_PRODUCTS_DIR}/${WRAPPER_NAME}.dSYM/Contents/Info.plist"
    echo "Setting $VERSION for $DSYM_PLIST"
    /usr/libexec/PlistBuddy -c "Set :CFBundleVersion $VERSION" "$DSYM_PLIST"
    /usr/libexec/PlistBuddy -c "Set :CFBundleShortVersionString $VERSION" "$DSYM_PLIST"
  fi
}