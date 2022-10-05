#!/usr/bin/env bash
#
# Compiles the Language Modeling Layer for common use in predictive text and autocorrective applications.
# Designed for optimal compatibility with the Keyman Suite.
#

# Exit on command failure and when using unset variables:
set -eu

# Include some helper functions from resources

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$(dirname "$THIS_SCRIPT")"

################################ Main script ################################

builder_check_color "$@"

builder_describe "Builds the lm-layer module" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  ":libraries  Targets all in-repo libraries that this module is dependent upon" \
  ":headless   A headless, Node-oriented version of the module useful for unit tests" \
  ":browser    The standard version of the module for in-browser use" \
  "--ci        Sets ${BUILDER_TERM_START}test${BUILDER_TERM_END} action to use CI-based test configurations & reporting"

builder_parse "$@"

# Exit status on invalid usage.
EX_USAGE=64
LMLAYER_OUTPUT=build

### CONFIGURE ACTIONS

do_configure() {
  # Check if Node.JS/npm is installed.
  verify_npm_setup
}

CONFIGURED=
if builder_start_action configure :libraries; then
  do_configure
  CONFIGURED=configure:libraries

  builder_finish_action success configure :libraries
fi


if builder_start_action configure :browser; then
  if [ -n "$CONFIGURED" ]; then
    echo "Configuration already completed in ${BUILDER_TERM_START}${CONFIGURED}${BUILDER_TERM_END}; skipping."
  else
    do_configure
    CONFIGURED=configure:browser
  fi
  builder_finish_action success configure :browser
fi

if builder_start_action configure :headless; then
  if [ -n "$CONFIGURED" ]; then
    echo "Configuration already completed in ${BUILDER_TERM_START}${CONFIGURED}${BUILDER_TERM_END}; skipping."
  else
    do_configure
    CONFIGURED=configure:headless
  fi

  builder_finish_action success configure :headless
fi

### CLEAN ACTIONS

# A nice, extensible method for -clean operations.  Add to this as necessary.
do_clean() {
  rm -rf "$LMLAYER_OUTPUT"
}

CLEANED=
if builder_start_action clean :libraries; then
  do_clean
  CLEANED=clean:libraries

  builder_finish_action success clean :libraries
fi

if builder_start_action clean :browser; then
  if [ -n "$CLEANED" ]; then
    echo "${BUILDER_TERM_START}clean${BUILDER_TERM_END} already completed as ${BUILDER_TERM_START}${CLEANED}${BUILDER_TERM_END}; skipping."
  else
    do_clean
    CLEANED=clean:browser
  fi
  builder_finish_action success clean :browser
fi

if builder_start_action clean :headless; then
  if [ -n "$CLEANED" ]; then
    echo "${BUILDER_TERM_START}clean${BUILDER_TERM_END} already completed as ${BUILDER_TERM_START}${CLEANED}${BUILDER_TERM_END}; skipping."
  else
    do_clean
    CLEANED=clean:headless
  fi

  builder_finish_action success clean :headless
fi

### BUILD ACTIONS

if builder_start_action build :libraries; then
  "$KEYMAN_ROOT/common/web/keyman-version/build.sh"
  "$KEYMAN_ROOT/common/web/lm-worker/build.sh"

  builder_finish_action success build :libraries
fi

# Builds the top-level JavaScript file for use in browsers
if builder_start_action build :browser; then
  npm run tsc -- -b ./browser.tsconfig.json

  builder_finish_action success build :browser
fi

# Builds the top-level JavaScript file for use on Node
if builder_start_action build :headless; then
  npm run tsc -- -b ./tsconfig.json || fail

  builder_finish_action success build :headless
fi

### TEST ACTIONS
# Note - the actual test setup is done in a separate test script, but it's easy
# enough to route the calls through.

TEST_OPTIONS=
if builder_has_option --ci; then
  TEST_OPTIONS=--ci
fi

if builder_start_action test :libraries; then
  ./unit_tests/test.sh test:libraries $TEST_OPTIONS

  builder_finish_action success test :libraries
fi

if builder_start_action test :headless; then
  ./unit_tests/test.sh test:headless $TEST_OPTIONS

  builder_finish_action success test :headless
fi

if builder_start_action test :browser; then
  ./unit_tests/test.sh test:browser $TEST_OPTIONS

  builder_finish_action success test :browser
fi