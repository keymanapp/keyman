#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script for Samples and Test projects

# shellcheck disable=SC2164
# shellcheck disable=SC1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/build/utils.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/android/android-actions.inc.sh"

################################ Main script ################################

builder_describe \
  "Build KMSample1, KMSample2, and Test projects" \
  "all            run all actions" \
  "clean          clean artifact directories" \
  "build          configure and build Keyman for Android samples"

builder_parse "$@"

cd "${KEYMAN_ROOT}/android"

function do_build() {
  builder_launch /android/build.sh  configure,build:engine,sample1,sample2,keyboardharness
}

if builder_has_action all; then
  android_clean_action
  do_build
else
  builder_run_action  clean   android_clean_action
  builder_run_action  build   do_build
fi
