#!/usr/bin/env bash
#
# Builds the include script for the current Keyman version.
#

# Exit on command failure and when using unset variables:
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# This script runs from its own folder
cd "$(dirname $THIS_SCRIPT)"

display_usage ( ) {
  echo "Usage: $0 [configure] [clean] [build] [test]"
  echo "          [--verbose|-v]"
  echo "       $0 -h|--help"
  echo
  echo "  headless               runs all headless unit tests (CI or local)"
  echo "  browser                runs all browser-based unit tests in CI mode"
  echo "  all                    runs all unit test sets in CI mode"
  echo "  ci                     runs selected unit tests via BrowserStack with CI-oriented reporting"
  echo "  local                  runs selected unit test sets locally with local-oriented reporting"
  echo "  browser-debug          runs all browser-based unit tests in persistent local mode."
}

test-headless ( ) {
  echo "Headless unit tests for this module have not yet been defined."
}

test-browser ( ) {
  KARMA_FLAGS=
  if [[ $# -eq 1  && $1 == "debug" ]]; then
    KARMA_CONFIG="manual.conf.js"
    KARMA_FLAGS="--no-single-run"
  elif [ $REPORT_STYLE == "local" ]; then
    KARMA_CONFIG="manual.conf.js"
  else
    KARMA_CONFIG="CI.conf.js"
  fi

  npm run karma -- start src/test/auto/browser/$KARMA_CONFIG "$KARMA_FLAGS"
}

################################ Main script ################################

# Yes, this is a test script, not a build script.  But why not use the new
# style anyway?

builder_init "headless browser all ci local browser-debug" "$@"

# TODO: build if out-of-date if test is specified
# TODO: configure if npm has not been run, and build is specified

# START - Script parameter configuration

if builder_has_action build >/dev/null; then
  builder_chosen_actions+=("all");
fi

if builder_has_action all >/dev/null; then
  builder_chosen_actions+=("headless");
  builder_chosen_actions+=("browser");
fi

if builder_has_action all-local >/dev/null; then
  builder_chosen_actions+=("headless");
  builder_chosen_actions+=("browser");
fi

if builder_has_action local >/dev/null; then
  REPORT_STYLE="local"
elif builder_has_action ci >/dev/null; then
  REPORT_STYLE="ci"
else
  # Attempt a TC-auto-detect.
  echo "Auto-detecting optimal testing style..."
  if [ ! -z "${TEAMCITY_VERSION-}" ]; then
    echo "TeamCity detected:  running in CI mode."
    builder_chosen_actions+=("ci");
    REPORT_STYLE="ci"
  else
    echo "TeamCity not detected:  running in local mode."
    builder_chosen_actions+=("local");
    REPORT_STYLE="local"
  fi
fi

# END - Script parameter configuration

if builder_has_action headless; then
  test-headless
  builder_report headless success
fi

if builder_has_action browser-debug; then
  test-browser debug
  builder_report browser-debug success
elif builder_has_action browser; then
  test-browser
  builder_report browser success
fi