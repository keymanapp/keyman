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

builder_describe "Runs all tests for the gesture-processor module" \
  "@../gesture-processor" \
  "test+" \
  ":headless   Runs headless user tests" \
  ":browser    Runs browser-based user tests" \
  "--ci        Uses CI-based test configurations & emits CI-friendly test reports"

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
  # During debugging, "--slow 0" allows reporting the duration of ALL tests, not just the ones that run long.
  # Can be useful... but probably shouldn't be the default.
  MOCHA_FLAGS=

  if [ $REPORT_STYLE == "ci" ]; then
    MOCHA_FLAGS="$MOCHA_FLAGS --reporter mocha-teamcity-reporter"
  fi

  # The currently-bundled declaration file for this package generates errors when compiling against it
  # with current tsc versions.
  rm -f "${KEYMAN_ROOT}/node_modules/promise-status-async/lib/index.d.ts"

  tsc -b ./src/test/tsconfig.json
  c8 mocha --recursive $MOCHA_FLAGS ./build/test/auto/headless/
}

test-browser ( ) {
  local WTR_DEBUG=
  local WTR_CONFIG=
  if [[ $# -eq 1  && $1 == "debug" ]]; then
    WTR_DEBUG=" --manual"
  elif [ $REPORT_STYLE != "local" ]; then
    WTR_CONFIG=.CI
  fi

  web-test-runner --config src/test/auto/browser/web-test-runner${WTR_CONFIG}.config.mjs ${WTR_DEBUG}
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