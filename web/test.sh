#!/usr/bin/env bash

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

################################ Main script ################################

builder_describe "Runs the Keyman Engine for Web unit-testing suites" \
  "@./src/tools/testing/recorder test:integrated" \
  "test+" \
  ":dom                  Runs DOM-oriented unit tests (reduced footprint, nothing browser-specific)" \
  ":integrated           Runs KMW's integration test suite" \
  "--inspect             Runs browser-based unit tests in an inspectable mode"

builder_parse "$@"

# Browser-based tests: common configs & kill-switches

# Select the right CONFIG file.
WTR_CONFIG=
if builder_is_ci_build; then
  WTR_CONFIG=.CI
fi

# Prepare the flags for the karma command.
WTR_INSPECT=
if builder_has_option --inspect; then
  WTR_INSPECT="--manual"
fi

# End common configs.

cd "${KEYMAN_ROOT}"

builder_run_action test:dom web-test-runner --config "web/src/test/auto/dom/web-test-runner${WTR_CONFIG}.config.mjs" ${WTR_INSPECT}

builder_run_action test:integrated web-test-runner --config "web/src/test/auto/integrated/web-test-runner${WTR_CONFIG}.config.mjs" ${WTR_INSPECT}
