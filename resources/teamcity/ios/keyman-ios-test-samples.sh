#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script for Keyman iOS/Test Samples and Test Projects

# shellcheck disable=SC2164
# shellcheck disable=SC1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

################################ Main script ################################

builder_describe \
  "Build samples and test projects for Keyman for iOS" \
  "all            build test samples"

builder_parse "$@"

# shellcheck disable=SC2154
cd "${KEYMAN_ROOT}/ios"

function _build_sample1() {
  builder_echo start "kmsample1" "Building KMSample1"
  builder_launch /ios/samples/KMSample1/build.sh clean configure build
  builder_echo end "kmsample1" success "Finished building KMSample1"
}

function _build_sample2() {
  builder_echo start "kmsample2" "Building KMSample2"
  builder_launch /ios/samples/KMSample2/build.sh clean configure build
  builder_echo end "kmsample2" success "Finished building KMSample2"
}

if builder_has_action all; then
  _build_sample1
  _build_sample2
fi
