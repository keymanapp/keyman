#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script for Keyman Developer on Linux and macOS

# shellcheck disable=SC2164
# shellcheck disable=SC1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/teamcity/developer/developer-actions.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-helpers.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-linux.inc.sh"

################################ Main script ################################

builder_describe \
  "Build Keyman Developer on Linux and macOS" \
  "all            run all actions" \
  "configure      install dependencies" \
  "build          build"

builder_parse "$@"

cd "${KEYMAN_ROOT}/developer/src"

function build_developer_action() {
  builder_echo start "build developer" "Building Keyman Developer"

  builder_launch /developer/src/build.sh configure build test

  builder_echo end "build developer" success "Finished building Keyman Developer"
}

if builder_is_windows; then
  builder_echo error "This script is intended to be run on Linux or macOS only."
  exit 1
fi

if builder_has_action all; then
  developer_install_dependencies_on_linux_action

  tc_set_variables_for_nvm
  tc_set_variables_for_emscripten

  build_developer_action
else
  builder_run_action  configure   developer_install_dependencies_on_linux_action

  tc_set_variables_for_nvm
  tc_set_variables_for_emscripten

  builder_run_action  build       build_developer_action
fi
