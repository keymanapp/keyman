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
  "--ci                  Set to utilize CI-based test configurations & reporting.  May not be set with $(builder_term --debug)."

builder_parse "$@"

# Browser-based tests: common configs & kill-switches

# DO_BROWSER_TEST_SUITE=true

# if [[ $VERSION_ENVIRONMENT == test ]]; then
#   # Implied: CONFIG=CI.conf.js because `-CI` parameter is passed.
#   #
#   # If we are running a TeamCity test build, for now, only run BrowserStack
#   # tests when on a PR branch with a title including "(web)" or with the label
#   # test-browserstack. This is because the BrowserStack tests are currently
#   # unreliable, and the false positive failures are masking actual failures.
#   #
#   # We do not run BrowserStack tests on master, beta, or stable-x.y test
#   # builds.
#   DO_BROWSER_TEST_SUITE=false
#   if builder_pull_get_details; then
#     if [[ $builder_pull_title =~ \(web\) ]] || builder_pull_has_label test-browserstack; then
#       DO_BROWSER_TEST_SUITE=true
#     fi
#   fi
# fi

# if [[ $DO_BROWSER_TEST_SUITE == false ]]; then
#   builder_warn "Skipping action test:engine - this CI build does not appear to be for a Web PR."
#   builder_finish_action success test:engine
#   exit 0
# fi

# Select the right CONFIG file.
WTR_CONFIG=
if builder_has_option --ci; then
  WTR_CONFIG=.CI
fi

# Prepare the flags for the karma command.
WTR_DEBUG=
if builder_is_debug_build; then
  WTR_DEBUG="--manual"
fi

# End common configs.

builder_run_action test:dom web-test-runner --config "src/test/auto/dom/web-test-runner${WTR_CONFIG}.config.mjs" ${WTR_DEBUG}

builder_run_action test:integrated web-test-runner --config "src/test/auto/integrated/web-test-runner${WTR_CONFIG}.config.mjs" ${WTR_DEBUG}
