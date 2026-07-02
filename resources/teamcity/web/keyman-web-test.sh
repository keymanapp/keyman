#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script for Keyman Web/Test

# shellcheck disable=SC2164
# shellcheck disable=SC1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/build/utils.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/web/web-actions.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-helpers.inc.sh"
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-linux.inc.sh"

################################ Main script ################################

builder_describe \
  "Run tests for native KeymanWeb" \
  "all            run all actions" \
  "configure      install dependencies" \
  "build          build Web + embedded" \
  "test           run native KeymanWeb tests and check build size"

builder_parse "$@"

cd "${KEYMAN_ROOT}/web"

function check_build_size_action() {
  builder_echo start "check build size" "Check build size"
  "${KEYMAN_ROOT}/web/ci.sh" validate-size
  builder_echo end "check build size" success "Finished checking build size"
}

if builder_has_action all; then
  web_install_dependencies_on_linux_action

  tc_set_variables_for_nvm

  web_build_action
  web_test_action
  check_build_size_action
else
  builder_run_action  configure   web_install_dependencies_on_linux_action

  tc_set_variables_for_nvm

  builder_run_action  build       web_build_action
  builder_run_action  test        web_test_action
  builder_run_action  test        check_build_size_action
fi
