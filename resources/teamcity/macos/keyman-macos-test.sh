#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script for Keyman macOS/Test
#
# Requires: env variable $MAC_BUILDAGENT_PASSWORD set

# shellcheck disable=SC2164
# shellcheck disable=SC1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/build/utils.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-mac.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/macos/macos-actions.inc.sh"

################################ Main script ################################

builder_describe \
  "Build Keyman for macOS" \
  "all            run all actions" \
  "clean          clean artifact directories" \
  "build          build Keyman for macOS"

builder_parse "$@"

cd "${KEYMAN_ROOT}/mac"

ba_mac_unlock_keychain

if builder_has_action all; then
  macos_clean_action
  macos_build_action
else
  builder_run_action  clean   macos_clean_action
  builder_run_action  build   macos_build_action
fi
