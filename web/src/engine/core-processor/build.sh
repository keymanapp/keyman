#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

SUBPROJECT_NAME=engine/core-processor

. "${KEYMAN_ROOT}/web/common.inc.sh"
. "${KEYMAN_ROOT}/resources/shellHelperFunctions.sh"

# ################################ Main script ################################

builder_describe "Keyman Core WASM integration" \
  "@/core:wasm" \
  "@/web/src/engine/common/web-utils" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "--ci+                     Set to utilize CI-based test configurations & reporting."

builder_describe_outputs \
  configure    "/web/src/engine/core-processor/src/import/core/km-core-interface.d.ts" \
  build        "/web/build/${SUBPROJECT_NAME}/lib/index.mjs"

builder_parse "$@"

#### Build action definitions ####

do_clean() {
  rm -rf "${KEYMAN_ROOT}/web/build/${SUBPROJECT_NAME}"
  rm -rf "src/import/"
}

do_configure() {
  verify_npm_setup

  mkdir -p "src/import/core/"
  # we don't need this file for release builds, but it's nice to have
  # for reference and auto-completion
  cp "${KEYMAN_ROOT}/core/build/wasm/${BUILDER_CONFIGURATION}/src/km-core-interface.d.ts" "src/import/core/"
}

copy_deps() {
  mkdir -p "${KEYMAN_ROOT}/web/build/${SUBPROJECT_NAME}/obj/import/core/"
  cp "${KEYMAN_ROOT}/core/build/wasm/${BUILDER_CONFIGURATION}/src/"km-core-interface.d.ts "${KEYMAN_ROOT}/web/build/${SUBPROJECT_NAME}/obj/import/core/"
  cp "${KEYMAN_ROOT}/core/build/wasm/${BUILDER_CONFIGURATION}/src/"km-core{.js,.wasm} "${KEYMAN_ROOT}/web/build/${SUBPROJECT_NAME}/obj/import/core/"
}

do_build () {
  copy_deps
  compile "${SUBPROJECT_NAME}"

  ${BUNDLE_CMD} "${KEYMAN_ROOT}/web/build/${SUBPROJECT_NAME}/obj/index.js" \
    --out         "${KEYMAN_ROOT}/web/build/${SUBPROJECT_NAME}/lib/index.mjs" \
    --format esm
}

builder_run_action clean      do_clean
builder_run_action configure  do_configure
builder_run_action build      do_build
