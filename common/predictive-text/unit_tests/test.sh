#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
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
  ":libraries  Runs unit tests for in-repo libraries used by this module"\
  ":headless   Runs this module's headless user tests" \
  ":browser    Runs this module's browser-based user tests" \
  "--ci        Uses CI-based test configurations & emits CI-friendly test reports"

# TODO: consider dependencies? ideally this will be test.inc.sh?

builder_parse "$@"

TEST_OPTS=
if builder_has_option --ci && builder_is_debug_build; then
  builder_die "Options --ci and --debug are incompatible."
elif builder_has_option --ci; then
  TEST_OPTS=--ci
fi

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

if builder_start_action test:libraries; then

  # Note:  these do not yet provide TeamCity-friendly-formatted test reports.
  # They do not have builder-based scripts, being run directly via npm package script.
  # So, for now, we add a text header to clarify what is running at each stage, in
  # addition to fair bit of `pushd` and `popd`.
  echo
  echo "### Running $(builder_term common/models/wordbreakers) tests"
  "$KEYMAN_ROOT/common/models/wordbreakers/build.sh" test $TEST_OPTS

  pushd "$KEYMAN_ROOT/common/models/templates"
  echo
  echo "### Running $(builder_term common/models/templates) tests"
  "$KEYMAN_ROOT/common/models/templates/build.sh" test $TEST_OPTS
  popd

  pushd "$KEYMAN_ROOT/common/web/lm-worker"
  echo
  echo "### Running ${BUILDER_TERM_START}common/web/lm-worker${BUILDER_TERM_END} tests"
  ./build.sh test $TEST_OPTS
  popd

  builder_finish_action success test:libraries
fi

if builder_start_action test:headless; then
  MOCHA_FLAGS=$FLAGS

  if builder_has_option --ci; then
    MOCHA_FLAGS="$MOCHA_FLAGS --reporter mocha-teamcity-reporter"
  fi

  mocha --recursive $MOCHA_FLAGS ./headless/*.js ./headless/**/*.js

  builder_finish_action success test:headless
fi

# If we are running a TeamCity test build, for now, only run BrowserStack
# tests when on a PR branch with a title including "(web)" or with the label
# test-browserstack. This is because the BrowserStack tests are currently
# unreliable, and the false positive failures are masking actual failures.
#
# We do not run BrowserStack tests on master, beta, or stable-x.y test
# builds.
if [[ $VERSION_ENVIRONMENT == test ]] && builder_has_action test:browser; then
  if builder_pull_get_details; then
    if ! ([[ $builder_pull_title =~ \(web\) ]] || builder_pull_has_label test-browserstack); then

      echo "Auto-skipping $(builder_term test:browser) for unrelated CI test build"
      exit 0
    fi
  fi
fi

get_browser_set_for_OS ( ) {
  if [[ $BUILDER_OS == mac ]]; then
    BROWSERS="--browsers Firefox,Chrome,Safari"
  elif [[ $BUILDER_OS == win ]]; then
    BROWSERS="--browsers Chrome"
  else
    BROWSERS="--browsers Firefox,Chrome"
  fi
}

if builder_start_action test:browser; then
  KARMA_FLAGS=$FLAGS
  KARMA_INFO_LEVEL="--log-level=warn"

  # if builder_has_option --ci; then
  #   KARMA_FLAGS="$KARMA_FLAGS --reporters teamcity,BrowserStack"
  #   KARMA_CONFIG="CI.conf.cjs"
  #   KARMA_INFO_LEVEL="--log-level=debug"
  # else
  #   KARMA_CONFIG="manual.conf.cjs"
  #   if builder_is_debug_build; then
  #     KARMA_FLAGS="$KARMA_FLAGS --no-single-run"
  #     KARMA_CONFIG="manual.conf.cjs"
  #     KARMA_INFO_LEVEL="--log-level=debug"

  #     echo
  #     echo "${COLOR_YELLOW}You must manually terminate this mode (CTRL-C) for the script to exit.${COLOR_RESET}"
  #     sleep 2
  #   fi
  # fi

  if [[ KARMA_CONFIG == "manual.conf.cjs" ]]; then
    get_browser_set_for_OS
  else
    BROWSERS=
  fi
  # karma start $KARMA_INFO_LEVEL $KARMA_FLAGS $BROWSERS ./in_browser/$KARMA_CONFIG
  web-test-runner --config in_browser/web-test-runner.config.mjs

  builder_finish_action success test:browser
fi
