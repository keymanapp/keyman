#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/web/common.inc.sh"
. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"

# BASE_DIR="/web/src/engine/osk/gesture-processor"
BUILD_DIR=/web/build/tools/testing/gesture-processor

################################ Main script ################################

builder_describe "Testing-oriented tools for the Gesture Processor module of web-based Keyman OSKs" \
  "@/common/tools/es-bundling   build" \
  "clean" \
  "build" \
  ":fixture       The HTML-element fixture and CSS fixture used for both user-testing and unit-testing" \
  ":recorder      The web page used for recording input sequences for use in unit-testing" \
  ":test-module   The TS library used to interface with the main gesture-processor module for tests"

builder_parse "$@"

builder_describe_outputs \
  configure            /node_modules \
  build:fixture        "${BUILD_DIR}/unit-test-resources/host-fixture.html" \
  build:recorder       "/web/tools/testing/gesture-processor/recorder/build/recorder.mjs" \
  build:test-module    "${BUILD_DIR}/unit-test-resources/lib/index.mjs"

# TODO: build if out-of-date if test is specified
# TODO: configure if npm has not been run, and build is specified

function do_clean_fixture() {
  rm -f "${KEYMAN_ROOT}/${BUILD_DIR}/unit-test-resources/host-fixture.html"
  rm -f "${KEYMAN_ROOT}/${BUILD_DIR}/unit-test-resources/gestureHost.css"
}

function do_clean_testmodule() {
  rm -rf "${KEYMAN_ROOT}/${BUILD_DIR}/unit-test-resources/*.ts*"
  rm -rf "${KEYMAN_ROOT}/${BUILD_DIR}/unit-test-resources/*.js*"
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
  mkdir -p "${KEYMAN_ROOT}/${BUILD_DIR}/unit-test-resources"
  ./host-fixture/extract-fixture.sh > "${KEYMAN_ROOT}/${BUILD_DIR}/unit-test-resources/host-fixture.html"
  cp ./host-fixture/gestureHost.css "${KEYMAN_ROOT}/${BUILD_DIR}/unit-test-resources/gestureHost.css"
}

function do_build_testmodule() {
  compile "" "${THIS_SCRIPT_PATH}/unit-test-resources" "${KEYMAN_ROOT}/${BUILD_DIR}/unit-test-resources"

  node_es_bundle "${KEYMAN_ROOT}/${BUILD_DIR}/unit-test-resources/obj/index.js" \
    --out        "${KEYMAN_ROOT}/${BUILD_DIR}/unit-test-resources/lib/index.mjs" \
    --format "esm"
}

builder_run_action clean:recorder     rm -rf ./recorder/build
builder_run_action clean:fixture      do_clean_fixture
builder_run_action clean:test-module  do_clean_testmodule
builder_run_action build:recorder     do_build_recorder
builder_run_action build:fixture      do_build_fixture
builder_run_action build:test-module  do_build_testmodule
