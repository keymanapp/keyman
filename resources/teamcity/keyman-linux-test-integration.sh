#!/usr/bin/env bash
# TC build script for Keyman Linux/Test:Integration
# shellcheck disable=SC2164
# shellcheck disable=SC1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/shellHelperFunctions.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-actions.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-linux.inc.sh"

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

if builder_has_action all; then
  linux_clean_action
  linux_install_dependencies_action
  linux_additional_test_dependencies_action
  set_variables_for_nvm
  linux_build_action
  linux_unit_tests_action
else
  builder_run_action  clean       linux_clean_action
  builder_run_action  configure   linux_install_dependencies_action
  builder_run_action  configure   linux_additional_test_dependencies_action

  set_variables_for_nvm

  builder_run_action  build       linux_build_action
  builder_run_action  test        linux_unit_tests_action
fi
