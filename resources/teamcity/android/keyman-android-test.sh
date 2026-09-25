#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script for Keyman Android/Test

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
  "Build and run tests for Keyman for Android" \
  "--fv           additionally build and Test FV Android" \
  "all            run all actions" \
  "clean          clean artifact directories" \
  "build          configure, build and test Keyman for Android" \
  "publish        publish symbols to Sentry (for release buildLevel)"

builder_parse "$@"

cd "${KEYMAN_ROOT}/android"

if builder_has_option --fv; then
  TARGETS="engine,app,fv"
else
  # shellcheck disable=SC2034
  TARGETS="engine,app"
fi

if builder_has_action all; then
  android_clean_action
  android_build_action "${TARGETS}"
  android_publish_symbols "${TARGETS}"
else
  builder_run_action  clean     android_clean_action
  builder_run_action  build     android_build_action "${TARGETS}"
  builder_run_action  publish   android_publish_symbols "${TARGETS}"
fi
