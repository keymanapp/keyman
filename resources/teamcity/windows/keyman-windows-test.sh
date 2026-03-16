#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script for Keyman Windows/Test

# shellcheck disable=SC2164
# shellcheck disable=SC1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/build/utils.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-helpers.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/windows/windows-actions.inc.sh"

################################ Main script ################################

builder_describe \
  "Run tests for Keyman for Windows" \
  "all            run all actions" \
  "build          build Keyman for Windows" \
  "test           run Keyman for Windows tests" \
  "publish        prepare files for distribution, publish symbols, and build installer module"

builder_parse "$@"

cd "${KEYMAN_ROOT}/windows/src"

if ! builder_is_windows; then
  builder_echo error "This script is intended to be run on Windows only."
  exit 1
fi

function windows_publish_action() {
  builder_echo start "publish windows" "Publishing Keyman for Windows"
  builder_launch /windows/build.sh publish
  windows_upload_symbols_to_sentry
  builder_echo end "publish windows" success "Finished publishing Keyman for Windows"
}

if builder_has_action all; then
  windows_build_action
  windows_test_action
  windows_publish_action
else
  builder_run_action  build       windows_build_action
  builder_run_action  test        windows_test_action
  builder_run_action  publish     windows_publish_action
fi

