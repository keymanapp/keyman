#!/usr/bin/env bash
#
# Compiles common TS-based utility functions for use among Keyman's codebase
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

BUNDLE_CMD="node $KEYMAN_ROOT/common/web/es-bundling/build/common-bundle.mjs"

################################ Main script ################################

builder_describe \
  "Compiles the web-oriented utility function module." \
  "@/common/web/keyman-version" \
  "@/common/web/es-bundling" \
  clean configure build test \
  "--ci    For use with action ${BUILDER_TERM_START}test${BUILDER_TERM_END} - emits CI-friendly test reports"

builder_describe_outputs \
  configure "/node_modules" \
  build     "/common/web/utils/build/obj/index.js"

builder_parse "$@"

function do_build() {
  tsc --build "$THIS_SCRIPT_PATH/tsconfig.json"

  # May be useful one day, for building a mass .d.ts for KMW as a whole.
  # So... tsc does declaration-bundling on its own pretty well, at least for local development.
  tsc --emitDeclarationOnly --outFile ./build/lib/index.d.ts

  # One of the functions (timedPromise) is quite helpful for automated testing, even in the DOM.
  # So, to make sure it's easily-accessible for the DOM-based tests...
  $BUNDLE_CMD    "${KEYMAN_ROOT}/common/web/utils/build/obj/index.js" \
    --out        "${KEYMAN_ROOT}/common/web/utils/build/lib/index.mjs" \
    --format esm
}

function do_test() {
  builder_heading "Running web-utils test suite"

  local FLAGS=
  if builder_has_option --ci; then
    echo "Replacing user-friendly test reports with CI-friendly versions."
    FLAGS="$FLAGS --reporter mocha-teamcity-reporter"
  fi

  c8 mocha --recursive $FLAGS ./src/test/
}

builder_run_action configure  verify_npm_setup
builder_run_action clean      rm -rf build/
builder_run_action build      do_build
builder_run_action test       do_test