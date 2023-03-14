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

# Temp-removed dependency:
# "@./src/tools/testing/recorder test:engine" \

builder_describe "Runs the Keyman Engine for Web unit-testing suites" \
  "test+" \
  ":engine               Runs the top-level Keyman Engine for Web unit tests" \
  ":libraries            Runs all unit tests for KMW's submodules.  Currently excludes predictive-text tests" \
  "--ci                  Set to utilize CI-based test configurations & reporting.  May not be set with $(builder_term --debug)." \
  "--reporters=REPORTERS Set to override the 'reporters' used by the unit testing engines" \
  "--browsers=BROWSERS   Set to override automatic browser selection for $(builder_term :engine) tests"

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
  if [[ $BUILDER_OS == mac ]]; then
      BROWSERS="--browsers Firefox,Chrome,Safari"
  elif [[ $BUILDER_OS == win ]]; then
      BROWSERS="--browsers Firefox,Chrome"
  else
      BROWSERS="--browsers Firefox,Chrome"
  fi
}

if builder_start_action test:engine; then
  if builder_has_option --ci && builder_is_debug_build; then
    builder_die "Options --ci and --debug are incompatible."
  fi

  if [[ $DO_BROWSER_TEST_SUITE == false ]]; then
    builder_warn "Skipping action test:engine - this CI build does not appear to be for a Web PR."
    builder_finish_action success test:engine
    exit 0
  fi

  # Auto-select browsers if not specified as an option
  if ! builder_has_option --browsers; then
    get_default_browser_set
  fi

  # Select the right CONFIG file.
  if builder_has_option --ci; then
    CONFIG=CI.conf.cjs
  else
    CONFIG=manual.conf.cjs
  fi

  # Build modernizr module
  modernizr -c src/test/auto/integrated/modernizr.config.json -d src/test/auto/integrated/modernizr.js

  # Prepare the flags for the karma command.
  KARMA_FLAGS=

  if builder_is_debug_build; then
    KARMA_FLAGS="$KARMA_FLAGS --no-single-run"
  fi

  if builder_has_option --reporters; then
    KARMA_FLAGS="$KARMA_FLAGS --reporters $REPORTERS"
  fi

  KARMA_EXT_FLAGS=
  if ! builder_has_option --ci; then
    KARMA_EXT_FLAGS="$KARMA_FLAGS --browsers $BROWSERS"
  fi

  karma start $KARMA_FLAGS "${KEYMAN_ROOT}/web/src/test/auto/dom/$CONFIG"
  karma start $KARMA_FLAGS $KARMA_EXT_FLAGS "${KEYMAN_ROOT}/web/src/test/auto/integrated/$CONFIG"

  builder_finish_action success test:engine
fi
