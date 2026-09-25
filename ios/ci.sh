#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/mac/mac.inc.sh"

builder_describe "Defines actions for use in CI-related iOS builds." \
  "build" \
  "prep-release"
  # For consideration:  clean               ("Cleanup Carthage Mess", "Clean XCode DerivedData Mess")
  # For consideration:  publish:testflight  ("Upload to TestFlight (PR area)")
  # For consideration:  publish:app-store   ("Push * to the App Store for Deployment")
  # For consideration:  publish:fastlane    (for matching release-build steps)

builder_parse "$@"

mac_verify_on_mac

function do_build() {
  builder_launch /ios/build.sh                         clean configure build:engine build:app --sim-artifact

  # Since we may be disabling it via build-agent environment variable at times...
  if [ ! -z "${RELEASE_OEM+x}" ]; then
    builder_launch /oem/firstvoices/ios/build.sh  clean configure build --sim-artifact
  fi
}

builder_run_action build          do_build
builder_run_action prep_release   builder_launch /ios/tools/prepRelease.sh