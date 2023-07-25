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
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$(dirname "$THIS_SCRIPT")"

################################ Main script ################################

#  "@../models/types" \ # is just a .d.ts, so there's nothing to actually BUILD.

builder_describe "Builds the lm-layer module" \
  "@/common/web/keyman-version" \
  "@/common/web/es-bundling" \
  "@/common/web/lm-worker" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "--ci        Sets $(builder_term test) action to use CI-based test configurations & reporting"

builder_describe_outputs \
  configure  /node_modules \
  build      /common/predictive-text/build/lib/web/index.mjs # is built by the final step.

builder_parse "$@"

function do_build() {
  # Builds the top-level JavaScript file for use on Node
  tsc -b ./tsconfig.all.json

  # esbuild-bundled products at this level are not intended to be used for anything but testing.
  node build-bundler.js
}

# Note - the actual test setup is done in a separate test script, but it's easy
# enough to route the calls through.
function do_test() {
  local TEST_OPTIONS=
  if builder_has_option --ci; then
    TEST_OPTIONS=--ci
  fi

  # We'll test the included libraries here for now.  At some point, we may wish
  # to establish a ci.sh script for predictive-text to handle this instead.
  ./unit_tests/test.sh test:libraries test:headless test:browser $TEST_OPTIONS
}

builder_run_action configure  verify_npm_setup
builder_run_action clean      rm -rf build/
builder_run_action build      do_build
builder_run_action test       do_test