#!/usr/bin/env bash

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$(dirname "$THIS_SCRIPT")"

################################ Main script ################################

builder_check_color "$@"

builder_describe "Runs the Keyman Engine for Web unit-testing suites" \
  "@./src/tools" \
  "@./ build:engine" \
  "test+" \
  ":engine               Runs the top-level Keyman Engine for Web unit tests" \
  ":libraries            Runs all unit tests for KMW's submodules.  Currently excludes predictive-text tests" \
  "--ci                  Set to utilize CI-based test configurations & reporting.  May not be set with ${BUILDER_TERM_START}--debug${BUILDER_TERM_END}." \
  "--reporters=REPORTERS Set to override the 'reporters' used by the unit testing engines" \
  "--browsers=BROWSERS   Set to override automatic browser selection for ${BUILDER_TERM_START}:engine${BUILDER_TERM_END} tests"

builder_parse "$@"

if builder_start_action test:libraries; then
  HEADLESS_FLAGS=

  if builder_has_option --ci; then
    HEADLESS_FLAGS=--ci
  fi

  # No --reporter option exists yet for the headless modules.

  $KEYMAN_ROOT/common/web/keyboard-processor/build.sh test $HEADLESS_FLAGS
  $KEYMAN_ROOT/common/web/input-processor/build.sh build:tools test $HEADLESS_FLAGS

  builder_finish_action success test:libraries
fi

# Browserstack or CI-based tests

DO_BROWSER_TEST_SUITE=true

if [[ $VERSION_ENVIRONMENT == test ]]; then
  # Implied: CONFIG=CI.conf.js because `-CI` parameter is passed.
  #
  # If we are running a TeamCity test build, for now, only run BrowserStack
  # tests when on a PR branch with a title including "(web)" or with the label
  # test-browserstack. This is because the BrowserStack tests are currently
  # unreliable, and the false positive failures are masking actual failures.
  #
  # We do not run BrowserStack tests on master, beta, or stable-x.y test
  # builds.
  DO_BROWSER_TEST_SUITE=false
  if builder_pull_get_details; then
    if [[ $builder_pull_title =~ \(web\) ]] || builder_pull_has_label test-browserstack; then
      DO_BROWSER_TEST_SUITE=true
    fi
  fi
fi

get_default_browser_set ( ) {
  # Default value, since it's the most general case/configuration to detect.
  local os_id="linux"

  # Subject to change with future improvements.
  if [[ "${OSTYPE}" = "darwin"* ]]; then
    os_id="mac"
  elif [[ "${OSTYPE}" = "msys" ]]; then
    os_id="win"
  elif [[ "${OSTYPE}" = "cygwin" ]]; then
    os_id="win"
  fi

  if [ $os_id = "mac" ]; then
      BROWSERS="--browsers Firefox,Chrome,Safari"
  elif [ $os_id = "win" ]; then
      BROWSERS="--browsers Firefox,Chrome,Edge"
  else
      BROWSERS="--browsers Firefox,Chrome"
  fi
}

if builder_start_action test:engine; then
  if builder_has_option --ci && builder_has_option --debug; then
    echo "${COLOR_RED}Options --ci and --debug are incompatible${COLOR_RESET}"
    exit 1
  fi

  if [[ DO_BROWSER_TEST_SUITE == false ]]; then
    echo "${COLOR_YELLOW}Skipping action test:engine - this CI build does not appear to be for a Web PR.${COLOR_YELLOW}"
    builder_finish_action success test:engine
    exit 0
  fi

  # Auto-select browsers if not specified as an option
  if ! builder_has_option --browsers; then
    get_default_browser_set
  fi

  # Select the right CONFIG file.
  if builder_has_option --ci; then
    CONFIG=CI.conf.js
  else
    CONFIG=manual.conf.js
  fi

  # Build modernizr module
  npm --no-color run modernizr -- -c src/test/auto/modernizr.config.json -d src/test/auto/modernizr.js

  # Prepare the flags for the karma command.
  KARMA_FLAGS=

  if builder_has_option --debug; then
    KARMA_FLAGS="$KARMA_FLAGS --no-single-run"
  fi

  if builder_has_option --reporters; then
    KARMA_FLAGS="$KARMA_FLAGS --reporters $REPORTERS"
  fi

  if ! builder_has_option --ci; then
    KARMA_FLAGS="$KARMA_FLAGS --browsers $BROWSERS"
  fi

  npm --no-color run karma -- start $KARMA_FLAGS src/test/auto/$CONFIG

  builder_finish_action success test:engine
fi