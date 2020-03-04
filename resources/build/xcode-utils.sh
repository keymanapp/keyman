#!/bin/bash

# This script will automatically have Xcode's build environment (and variables),
# so there's no need to do anything extra to fetch them.

# Some fun details for interacting with Xcode:
#  - echo "warning: foo" will generate an actual compile warning within Xcode:  "foo"
#  - echo "error: bar" will likewise generate a compile error within Xcode: "bar"
#
# As appropriate, make sure to take advantage of this feature!

# Build Phase:  "Set Bundle Versions"
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

# Build Phase:  Upload dSYM (debug) files to Sentry
# Takes one parameter - the SENTRY_PROJECT target.  The other parameter needs to be set as an "input file" within Xcode.
function phaseSentryDsymUpload() {

  if [[ $# -gt 0 ]]; then
    SENTRY_PROJECT_TARGET=$1
  else
    echo "error: SENTRY_PROJECT parameter was not provided to Sentry upload utility function."
    exit 1
  fi

  # Provides the needed definition for SENTRY_AUTH_TOKEN from TC configuration settings, since it is otherwise
  # not forwarded (privately) through Xcode.
  echo "warning: TODO: change .zshrc defintion of SENTRY_AUTH_TOKEN to something not requiring local configuration for each build agent"
  source ~/.zshrc

  # Reference for this detection check for the input file: https://www.iosdev.recipes/xcode/input-output-files/
  if [ -z $SCRIPT_INPUT_FILE_0 ]; then
    echo "error:  Run Script must have an input file set: \${DWARF_DSYM_FOLDER_PATH}/\${DWARF_DSYM_FILE_NAME}/Contents/Resources/DWARF/\${TARGET_NAME}"
  fi

  if [ -z $SENTRY_AUTH_TOKEN ]; then
    echo "warning:  Cannot successfully upload the dSYM to sentry if a Sentry auth token is not provided."
  fi

  # The remaining update logic seen here was auto-generated at https://sentry.keyman.com/keyman/keyman-ios/getting-started/cocoa-swift/
  if which sentry-cli >/dev/null; then
    export SENTRY_URL="https://sentry.keyman.com"
    export SENTRY_ORG=keyman
    export SENTRY_PROJECT=$SENTRY_PROJECT_TARGET
    ERROR=$(sentry-cli upload-dif "$DWARF_DSYM_FOLDER_PATH" 2>&1 >/dev/null)
    if [ ! $? -eq 0 ]; then
      echo "warning: sentry-cli - $ERROR"
    fi
  else
    echo "warning: sentry-cli not installed; please run `brew install getsentry/tools/sentry-cli` to remedy"
  fi
}