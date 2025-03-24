#!/usr/bin/env bash
#
# Compiles the Language Modeling Layer for common use in predictive text and autocorrective applications.
# Designed for optimal compatibility with the Keyman Suite.
#
# Include some helper functions from resources

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

BUNDLE_CMD="node $KEYMAN_ROOT/web/src/tools/es-bundling/build/common-bundle.mjs"

################################ Main script ################################

builder_describe "Builds the lm-layer module" \
  "@/common/web/keyman-version" \
  "@/common/web/types" \
  "@/web/src/tools/es-bundling" \
  "@/web/src/engine/predictive-text/worker-thread" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "--ci        Sets $(builder_term test) action to use CI-based test configurations & reporting"

builder_describe_outputs \
  configure  /node_modules \
  build      /web/src/engine/predictive-text/worker-main/build/lib/web/index.mjs # is built by the final step.

builder_parse "$@"

function do_configure() {
  verify_npm_setup

  # Configure Web browser-engine testing environments.  As is, this should only
  # make changes when we update the dependency, even on our CI build agents.
  playwright install
}

function do_build() {
  # Builds the top-level JavaScript file for use on Node
  tsc -b ./tsconfig.all.json

  # esbuild-bundled products at this level are not intended to be used for anything but testing.
  $BUNDLE_CMD    "${KEYMAN_ROOT}/web/src/engine/predictive-text/worker-main/build/obj/web/index.js" \
    --out        "${KEYMAN_ROOT}/web/src/engine/predictive-text/worker-main/build/lib/web/index.mjs" \
    --format esm
}

# Note - the actual test setup is done in a separate test script, but it's easy
# enough to route the calls through.
function do_test() {
  local TEST_OPTIONS=
  if builder_has_option --ci; then
    # We'll test the included libraries here for now.  At some point, we may wish
    # to establish a ci.sh script for predictive-text to handle this instead.
    ./unit_tests/test.sh test:libraries test:headless test:browser --ci
  else
    # If we're not in --ci mode, then this doesn't need to trigger the sibling projects' tests.
    ./unit_tests/test.sh test:headless test:browser
  fi
}

builder_run_action configure  do_configure
builder_run_action clean      rm -rf build/
builder_run_action build      do_build
builder_run_action test       do_test