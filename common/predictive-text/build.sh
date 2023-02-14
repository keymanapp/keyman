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

# TODO: once these modules are builder-based, reference here too:
#  "@../models/templates" \
#  "@../models/types" \
#  "@../models/wordbreakers"

builder_describe "Builds the lm-layer module" \
  "@../web/keyman-version" \
  "@../web/lm-worker" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  ":headless   A headless, Node-oriented version of the module useful for unit tests" \
  ":browser    The standard version of the module for in-browser use" \
  "--ci        Sets $(builder_term test) action to use CI-based test configurations & reporting"

builder_describe_outputs \
  configure           /node_modules \
  configure:headless  /node_modules \
  configure:browser   /node_modules \
  build:headless      build/headless.js \
  build:browser       build/index.js

builder_parse "$@"

### CONFIGURE ACTIONS

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

### CLEAN ACTIONS

if builder_start_action clean; then
  rm -rf build/
  builder_finish_action success clean
fi

### BUILD ACTIONS

# Builds the top-level JavaScript file for use in browsers
if builder_start_action build:browser; then
  npm run tsc -- -b ./browser.tsconfig.json

  builder_finish_action success build:browser
fi

# Builds the top-level JavaScript file for use on Node
if builder_start_action build:headless; then
  npm run tsc -- -b ./tsconfig.json

  builder_finish_action success build:headless
fi

### TEST ACTIONS
# Note - the actual test setup is done in a separate test script, but it's easy
# enough to route the calls through.

TEST_OPTIONS=
if builder_has_option --ci; then
  TEST_OPTIONS=--ci
fi

if builder_start_action test:headless; then
  # We'll test the included libraries here for now, at least until we have
  # converted their builds to builder scripts
  ./unit_tests/test.sh test:libraries test:headless $TEST_OPTIONS

  builder_finish_action success test:headless
fi

if builder_start_action test:browser; then
  ./unit_tests/test.sh test:browser $TEST_OPTIONS

  builder_finish_action success test:browser
fi
