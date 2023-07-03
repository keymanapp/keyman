#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

# Include our resource functions; they're pretty useful!
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# Please note that this build script (understandably) assumes that it is running on Mac OS X.
verify_on_mac

builder_describe "Defines actions for use in CI-related iOS builds." \
  "build" \
  "prep-release"
  # For consideration:  clean               ("Cleanup Carthage Mess", "Clean XCode DerivedData Mess")
  # For consideration:  publish:testflight  ("Upload to TestFlight (PR area)")
  # For consideration:  publish:app-store   ("Push * to the App Store for Deployment")
  # For consideration:  publish:fastlane    (for matching release-build steps)

builder_parse "$@"

function do_build() {
  ./build.sh                         clean configure build:engine build:app --sim-artifact

  # Since we may be disabling it via build-agent environment variable at times...
  if [ ! -z "${RELEASE_OEM+x}" ]; then
    "$KEYMAN_ROOT/oem/firstvoices/ios/build.sh"  clean configure build --sim-artifact
  fi
}

builder_run_action build          do_build
# The script called below isn't builder-based, but... eh, "if it ain't broke."
builder_run_action prep_release   tools/prepRelease.sh