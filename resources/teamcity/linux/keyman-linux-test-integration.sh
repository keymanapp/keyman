#!/usr/bin/env bash
# TC build script for Keyman Linux/Test:Integration
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
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-linux.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/linux/linux-actions.inc.sh"

################################ Main script ################################

builder_describe \
  "Runs unit plus integration tests." \
  "all            run all actions" \
  "clean          remove all build artifacts" \
  "configure      install dependencies" \
  "build          make a release build" \
  "test           run unit tests"

builder_parse "$@"

cd "${KEYMAN_ROOT}/linux"

# Run the clean action in the `linux` directory.`
function clean_action() {
  builder_heading "Cleaning up"
  # shellcheck disable=SC2154
  builder_launch /linux/build.sh clean
}

if builder_has_action all; then
  clean_action
  linux_install_dependencies_action
  linux_additional_test_dependencies_action
  tc_set_variables_for_nvm
  # shellcheck disable=SC2119
  linux_build_action
  # shellcheck disable=SC2119
  linux_unit_tests_action
else
  builder_run_action  clean       clean_action
  builder_run_action  configure   linux_install_dependencies_action
  builder_run_action  configure   linux_additional_test_dependencies_action

  tc_set_variables_for_nvm

  builder_run_action  build       linux_build_action
  builder_run_action  test        linux_unit_tests_action
fi
