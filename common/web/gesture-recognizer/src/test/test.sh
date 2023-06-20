#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. $KEYMAN_ROOT/resources/shellHelperFunctions.sh

# This script runs from its own folder
cd "$(dirname $THIS_SCRIPT)"

################################ Main script ################################

builder_describe "Runs all tests for the gesture-recognizer module" \
  "@/common/web/gesture-recognizer" \
  "test+" \
  ":headless   Runs headless user tests" \
  ":browser    Runs browser-based user tests" \
  "--ci        Uses CI-based test configurations & emits CI-friendly test reports" \
  "--debug,-d  Activates developer-friendly debug mode for unit tests where applicable"

builder_parse "$@"

# TODO: build if out-of-date if test is specified
# TODO: configure if npm has not been run, and build is specified

# START - Script parameter configuration
REPORT_STYLE="local"  # Default setting.

if builder_has_option --ci; then
  REPORT_STYLE="ci"

  echo "Replacing user-friendly test reports & configurations with CI-friendly versions."
fi

# END - Script parameter configuration

test-headless ( ) {
  MOCHA_FLAGS=

  if [ $REPORT_STYLE == "ci" ]; then
    MOCHA_FLAGS="--reporter mocha-teamcity-reporter"
  fi

  mocha --recursive $MOCHA_FLAGS ./auto/headless/
}

test-browser ( ) {
  KARMA_FLAGS=
  if [[ $# -eq 1  && $1 == "debug" ]]; then
    KARMA_CONFIG="manual.conf.cjs"
    KARMA_FLAGS="--no-single-run"
  elif [ $REPORT_STYLE == "local" ]; then
    KARMA_CONFIG="manual.conf.cjs"
  else
    KARMA_CONFIG="CI.conf.cjs"
  fi

  karma start auto/browser/$KARMA_CONFIG "$KARMA_FLAGS"
}

if builder_start_action test:headless; then
  test-headless
  builder_finish_action success test:headless
fi

if builder_start_action test:browser; then
  if builder_has_option --debug; then
    echo "Running browser-based unit tests in debug-mode configuration..."
    echo
    echo "${COLOR_YELLOW}You must manually terminate this mode (CTRL-C) for the script to exit.${COLOR_RESET}"
    sleep 2
    test-browser debug
  else
    test-browser
  fi
  builder_finish_action success test:browser
fi