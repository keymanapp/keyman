#!/usr/bin/env bash
# TC build script for Keyman Linux/Test
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
  "Tests install build, making a source package and runs unit tests." \
  "all            run all actions" \
  "configure      install dependencies" \
  "build          make a release build" \
  "test           run unit tests" \
  "publish        make a source tarball"

builder_parse "$@"

cd "${KEYMAN_ROOT}/linux"

function make_source_tarball_action() {
  if builder_is_ci_build_level_release; then
    builder_echo start "make source tarball" "Make source tarball"
    rm -rf dist
    PKG_CONFIG_PATH="${KEYMAN_ROOT}/core/build/arch/release/meson-private" "${KEYMAN_ROOT}/linux/scripts/dist.sh"
    builder_echo end "make source tarball" success "Finished making source tarball"
  else
    builder_echo "Skipping source tarball creation - not release build level"
  fi
}

if builder_has_action all; then
  linux_install_dependencies_action
  linux_additional_test_dependencies_action

  tc_set_variables_for_nvm

  linux_build_action --coverage
  linux_unit_tests_action --coverage --report --no-integration
  make_source_tarball_action
else
  builder_run_action  configure   linux_install_dependencies_action
  builder_run_action  configure   linux_additional_test_dependencies_action

  tc_set_variables_for_nvm

  builder_run_action  build       linux_build_action --coverage
  builder_run_action  test        linux_unit_tests_action --coverage --report --no-integration
  builder_run_action  publish     make_source_tarball_action
fi
