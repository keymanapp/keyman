#!/usr/bin/env bash
# TC build script for Keyman Linux/Test
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
  "Tests install build, making a source package and runs unit tests." \
  "all            run all actions" \
  "configure      install dependencies" \
  "build          make a release build" \
  "test           run unit tests" \
  "publish        make a source tarball"

builder_parse "$@"

cd "${KEYMAN_ROOT}/linux"

function make_source_tarball_action() {
  builder_echo start "make source tarball" "Make source tarball"
  rm -rf dist
  make tmpsources
  builder_echo end "make source tarball" success "Finished making source tarball"
}

if builder_has_action all; then
  linux_install_dependencies_action
  linux_additional_test_dependencies_action

  set_variables_for_nvm

  linux_build_action --coverage
  linux_unit_tests_action --coverage --report --no-integration
  make_source_tarball_action
else
  builder_run_action  configure   linux_install_dependencies_action
  builder_run_action  configure   linux_additional_test_dependencies_action

  set_variables_for_nvm

  builder_run_action  build       linux_build_action --coverage
  builder_run_action  test        linux_unit_tests_action --coverage --report --no-integration
  builder_run_action  publish     make_source_tarball_action
fi
