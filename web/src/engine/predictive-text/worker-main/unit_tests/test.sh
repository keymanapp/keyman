#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"

################################ Main script ################################

# Defaults
FLAGS=""

builder_describe "Runs all tests for the language-modeling / predictive-text layer module" \
  "configure" \
  "test+" \
  ":headless   Runs this module's headless user tests" \
  ":browser    Runs this module's browser-based user tests" \
  "--inspect   Runs browser-based tests in a locally-inspectable mode"

# TODO: consider dependencies? ideally this will be test.inc.sh?

builder_parse "$@"

function do_test_headless() {
  MOCHA_FLAGS=(${FLAGS})

  if builder_is_running_on_teamcity; then
    MOCHA_FLAGS+=(--reporter "${KEYMAN_ROOT}/common/test/resources/mocha-teamcity-reporter/teamcity.cjs" --reporter-options parentFlowId="unit_tests")
    echo "##teamcity[flowStarted flowId='unit_tests']"
  fi

  mocha --recursive "${MOCHA_FLAGS[@]}" ./headless/*.js ./headless/**/*.js

  if builder_is_running_on_teamcity; then
    # we're running in TeamCity
    echo "##teamcity[flowFinished flowId='unit_tests']"
  fi
}

function do_test_browser() {
  WTR_CONFIG=
  WTR_INSPECT=

  if builder_is_ci_build; then
    WTR_CONFIG=.CI
  fi

  if builder_has_option --inspect; then
    WTR_INSPECT=" --manual"
  fi

  web-test-runner --config in_browser/web-test-runner${WTR_CONFIG}.config.mjs ${WTR_INSPECT}
}

builder_run_action configure      node_select_version_and_npm_ci
builder_run_action test:headless  do_test_headless
builder_run_action test:browser   do_test_browser
