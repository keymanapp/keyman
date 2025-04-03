#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

SUBPROJECT_NAME=engine/js-processor

. "${KEYMAN_ROOT}/web/common.inc.sh"
. "${KEYMAN_ROOT}/resources/shellHelperFunctions.sh"

# ################################ Main script ################################

builder_describe "Builds configuration subclasses used by the Keyman Engine for Web (KMW)." \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "--ci+                     Set to utilize CI-based test configurations & reporting."

builder_describe_outputs \
  configure    "/node_modules" \
  build        "/web/build/${SUBPROJECT_NAME}/lib/index.mjs"

builder_parse "$@"

#### Build action definitions ####

do_build () {
  compile "${SUBPROJECT_NAME}"

  ${BUNDLE_CMD} "${KEYMAN_ROOT}/web/build/${SUBPROJECT_NAME}/obj/index.js" \
    --out         "${KEYMAN_ROOT}/web/build/${SUBPROJECT_NAME}/lib/index.mjs" \
    --format esm
}

do_test() {
  test-headless "${SUBPROJECT_NAME}" ""
  test-headless-typescript "${SUBPROJECT_NAME}"
}

builder_run_action configure  verify_npm_setup
builder_run_action clean      rm -rf "${KEYMAN_ROOT}/web/build/${SUBPROJECT_NAME}"
builder_run_action build      do_build
builder_run_action test       do_test
