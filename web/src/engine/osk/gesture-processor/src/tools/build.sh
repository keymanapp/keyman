#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/web/common.inc.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

BUNDLE_CMD="node $KEYMAN_ROOT/common/tools/es-bundling/build/common-bundle.mjs"

BASE_DIR="/web/src/engine/osk/gesture-processor"
BUILD_DIR="${BASE_DIR}/build"

################################ Main script ################################

builder_describe "Testing-oriented tools for the Gesture Processor module of web-based Keyman OSKs" \
  "clean" \
  "build" \
  ":fixture       The HTML-element fixture and CSS fixture used for both user-testing and unit-testing" \
  ":recorder      The web page used for recording input sequences for use in unit-testing" \
  ":test-module   The TS library used to interface with the main gesture-processor module for tests"

builder_parse "$@"

builder_describe_outputs \
  configure            /node_modules \
  build:fixture        "${BUILD_DIR}/tools/host-fixture.html" \
  build:recorder       "${BASE_DIR}/src/tools/recorder/build/recorder.mjs" \
  build:test-module    "${BUILD_DIR}/tools/lib/index.mjs"

# TODO: build if out-of-date if test is specified
# TODO: configure if npm has not been run, and build is specified

function do_clean_fixture() {
  rm -f ../../build/tools/host-fixture.html
  rm -f ../../build/tools/gestureHost.css
}

function do_clean_testmodule() {
  rm -rf ../../build/tools/*.ts*
  rm -rf ../../build/tools/*.js*
}

function do_build_recorder() {
  if [[ ! -d recorder/build ]]; then
    mkdir -p recorder/build
  fi
  cp recorder/src/pageStyle.css    recorder/build/pageStyle.css
  cp recorder/src/recorder.mjs     recorder/build/recorder.mjs

  cp host-fixture/gestureHost.css  recorder/build/gestureHost.css

  # Thanks to https://stackoverflow.com/a/10107668 for this tidbit.
  # Searches for FIXTURE_TARGET above and replaces it with the actual fixture!
  pushd recorder >/dev/null
  node update-index.cjs build/index.html
  popd >/dev/null
}

function do_build_fixture() {
  if [[ ! -d ../../build/tools ]]; then
    mkdir -p ../../build/tools
  fi
  ./host-fixture/extract-fixture.sh > ../../build/tools/host-fixture.html
  cp ./host-fixture/gestureHost.css ../../build/tools/gestureHost.css
}

function do_build_testmodule() {
  compile "" "${THIS_SCRIPT_PATH}/unit-test-resources" "${KEYMAN_ROOT}/${BUILD_DIR}/tools"

  $BUNDLE_CMD    "${KEYMAN_ROOT}/${BUILD_DIR}/tools/obj/index.js" \
    --out        "${KEYMAN_ROOT}/${BUILD_DIR}/tools/lib/index.mjs" \
    --format "esm"
}

builder_run_action clean:recorder     rm -rf ./recorder/build
builder_run_action clean:fixture      do_clean_fixture
builder_run_action clean:test-module  do_clean_testmodule
builder_run_action build:recorder     do_build_recorder
builder_run_action build:fixture      do_build_fixture
builder_run_action build:test-module  do_build_testmodule
