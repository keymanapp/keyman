#!/usr/bin/env bash
#
# Compiles common TS-based utility functions for use among Keyman's codebase
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "${KEYMAN_ROOT}/web/common.inc.sh"
. "${KEYMAN_ROOT}/resources/build/utils.inc.sh"
. "${KEYMAN_ROOT}/resources/build/node.inc.sh"

SUBPROJECT_NAME=common/web-utils
BUILD_DIR="/web/build/common/web-utils"

################################ Main script ################################

builder_describe \
  "Compiles the web-oriented utility function module." \
  "@/common/tools/es-bundling   build" \
  "@/common/web/keyman-version  build" \
  "@/common/web/types           build" \
  clean configure build test

builder_describe_outputs \
  configure "/node_modules" \
  build     "${BUILD_DIR}/obj/index.js"

builder_parse "$@"

function do_build() {
  compile "${SUBPROJECT_NAME}"

  # May be useful one day, for building a mass .d.ts for KMW as a whole.
  # So... tsc does declaration-bundling on its own pretty well, at least for local development.
  tsc --emitDeclarationOnly --outFile "${KEYMAN_ROOT}/${BUILD_DIR}/lib/index.d.ts"
  # One of the functions (timedPromise) is quite helpful for automated testing, even in the DOM.
  # So, to make sure it's easily-accessible for the DOM-based tests...
  node_es_bundle "${KEYMAN_ROOT}/${BUILD_DIR}/obj/index.js" \
    --out        "${KEYMAN_ROOT}/${BUILD_DIR}/lib/index.mjs" \
    --format esm
}

function do_test() {
  builder_heading "Running web-utils test suite"

  local FLAGS=()
  if builder_is_running_on_teamcity; then
    echo "Replacing user-friendly test reports with CI-friendly versions."
    FLAGS+=(--reporter "${KEYMAN_ROOT}/common/test/resources/mocha-teamcity-reporter/teamcity.cjs" --reporter-options parentFlowId="unit_tests")
    echo "##teamcity[flowStarted flowId='unit_tests']"
  fi

  c8 mocha --recursive "${FLAGS[@]}" ./src/tests/

  if builder_is_running_on_teamcity; then
    # we're running in TeamCity
    echo "##teamcity[flowFinished flowId='unit_tests']"
  fi
}

builder_run_action configure  node_select_version_and_npm_ci
builder_run_action clean      rm -rf build/
builder_run_action build      do_build
builder_run_action test       do_test
