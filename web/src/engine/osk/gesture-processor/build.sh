#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

SUBPROJECT_NAME=engine/osk/gesture-processor

. "${KEYMAN_ROOT}/web/common.inc.sh"
. "${KEYMAN_ROOT}/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"

BUNDLE_CMD="node $KEYMAN_ROOT/web/src/tools/es-bundling/build/common-bundle.mjs"

BUILD_DIR="/web/src/engine/osk/gesture-processor/build"

################################ Main script ################################

builder_describe "Builds the gesture-recognition model for Web-based on-screen keyboards" \
  "@/web/src/tools/es-bundling build" \
  "@/web/src/engine/common/web-utils build" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  ":module" \
  ":tools  tools for testing & developing test resources for this module"

builder_describe_outputs \
  configure        /node_modules \
  build:module     "${BUILD_DIR}/lib/index.mjs" \
  build:tools      "${BUILD_DIR}/tools/lib/index.mjs"

builder_parse "$@"

# TODO: build if out-of-date if test is specified
# TODO: configure if npm has not been run, and build is specified

function do_configure() {
  node_select_version_and_npm_ci
}

function do_build_module() {
  # Build
  tsc --build $builder_verbose

  $BUNDLE_CMD    "${KEYMAN_ROOT}/${BUILD_DIR}/obj/index.js" \
    --out        "${KEYMAN_ROOT}/${BUILD_DIR}/lib/index.mjs" \
    --format esm
}

function do_test_tools() {
  if ! builder_has_action test:module; then
    echo "The $(builder_term test:tools) action is currently a no-op."
  fi
}

builder_run_action configure     do_configure
builder_run_action clean         rm -rf build/
builder_run_action build:module  do_build_module
builder_run_action build:tools   builder_launch /web/src/engine/osk/gesture-processor/src/tools/build.sh build
builder_run_action test:module   test-headless-typescript "${SUBPROJECT_NAME}"
builder_run_action test:tools    do_test_tools
