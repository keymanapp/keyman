#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

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

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

if builder_start_action test:headless; then
  MOCHA_FLAGS=$FLAGS

  if builder_is_ci_build; then
    MOCHA_FLAGS="$MOCHA_FLAGS --reporter mocha-teamcity-reporter"
  fi

  mocha --recursive $MOCHA_FLAGS ./headless/*.js ./headless/**/*.js

  builder_finish_action success test:headless
fi

if builder_start_action test:browser; then
  WTR_CONFIG=
  WTR_INSPECT=

  if builder_is_ci_build; then
    WTR_CONFIG=.CI
  fi

  if builder_has_option --inspect; then
    WTR_INSPECT=" --manual"
  fi

  web-test-runner --config in_browser/web-test-runner${WTR_CONFIG}.config.mjs ${WTR_INSPECT}

  builder_finish_action success test:browser
fi
