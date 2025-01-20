#!/usr/bin/env bash
#
# Compiles common TS-based utility functions for use among Keyman's codebase
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "${KEYMAN_ROOT}/web/common.inc.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

SUBPROJECT_NAME=engine/common/web-utils
BUILD_DIR="/web/src/engine/common/web-utils/build"

BUNDLE_CMD="node $KEYMAN_ROOT/web/src/tools/es-bundling/build/common-bundle.mjs"

################################ Main script ################################

builder_describe \
  "Compiles the web-oriented utility function module." \
  "@/common/web/keyman-version" \
  "@/web/src/tools/es-bundling" \
  clean configure build test \
  "--ci    For use with action ${BUILDER_TERM_START}test${BUILDER_TERM_END} - emits CI-friendly test reports"

builder_describe_outputs \
  configure "/node_modules" \
  build     "${BUILD_DIR}/obj/index.js"

builder_parse "$@"

function do_build() {
  tsc --build $builder_verbose "$THIS_SCRIPT_PATH/tsconfig.json"

  # May be useful one day, for building a mass .d.ts for KMW as a whole.
  # So... tsc does declaration-bundling on its own pretty well, at least for local development.
  tsc --emitDeclarationOnly --outFile "${KEYMAN_ROOT}/${BUILD_DIR}/lib/index.d.ts"

  # One of the functions (timedPromise) is quite helpful for automated testing, even in the DOM.
  # So, to make sure it's easily-accessible for the DOM-based tests...
  $BUNDLE_CMD    "${KEYMAN_ROOT}/${BUILD_DIR}/obj/index.js" \
    --out        "${KEYMAN_ROOT}/${BUILD_DIR}/lib/index.mjs" \
    --format esm
}

function do_test() {
  builder_heading "Running web-utils test suite"

  local FLAGS=
  if builder_has_option --ci; then
    echo "Replacing user-friendly test reports with CI-friendly versions."
    FLAGS="$FLAGS --reporter mocha-teamcity-reporter"
  fi

  c8 mocha --recursive $FLAGS ./src/tests/
}

builder_run_action configure  verify_npm_setup
builder_run_action clean      rm -rf build/
builder_run_action build      do_build
builder_run_action test       do_test
