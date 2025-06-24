#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script for Samples and Test projects

# shellcheck disable=SC2164
# shellcheck disable=SC1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/shellHelperFunctions.sh"
. "${KEYMAN_ROOT}/resources/teamcity/android/android-actions.inc.sh"

################################ Main script ################################

builder_describe \
  "Build KMSample1, KMSample2, and Test projects" \
  "all            run all actions" \
  "clean          clean artifact directories" \
  "build          configure, build and test Keyman for Android"

builder_parse "$@"

cd "${KEYMAN_ROOT}/android"

function _build_kmea() {
  builder_echo start "build" "Configuring and building KMEA"
  "${KEYMAN_ROOT}/android/build.sh" configure:engine build:engine --debug --ci
  builder_echo end "build" success "Finished configuring and building KMEA"
}

function _build_kmsample1() {
  builder_echo start "build" "Building KMSample1"
  "${KEYMAN_ROOT}/android/build.sh" build:sample1 --debug --ci
  builder_echo end "build" success "Finished building KMSample1"
}

function _build_kmsample2() {
  builder_echo start "build" "Building KMSample2"
  "${KEYMAN_ROOT}/android/build.sh" build:sample2 --debug --ci
  builder_echo end "build" success "Finished building KMSample2"
}

function _build_keyboardharness() {
  builder_echo start "build" "Building KeyboardHarness"
  "${KEYMAN_ROOT}/android/build.sh" build:keyboardharness --debug --ci
  builder_echo end "build" success "Finished building KeyboardHarness"
}

function do_build() {
  _build_kmea
  _build_kmsample1
  _build_kmsample2
  _build_keyboardharness
}

if builder_has_action all; then
  android_clean_action
  do_build
else
  builder_run_action  clean   android_clean_action
  builder_run_action  build   do_build
fi
