#!/usr/bin/env bash
#
# Compile KeymanWeb's 'keyboard' module, one of the components of Web's 'core' module.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

SUBPROJECT_NAME=engine/keyboard

. "${KEYMAN_ROOT}/web/common.inc.sh"
. "${KEYMAN_ROOT}/resources/shellHelperFunctions.sh"

################################ Main script ################################

builder_describe \
  "Compiles the web-oriented utility function module." \
  "@/common/web/keyman-version" \
  "@/common/web/types" \
  "@/web/src/tools/testing/recorder-core  test" \
  "@/web/src/tools/es-bundling" \
  "@/web/src/engine/common/web-utils" \
  "@/web/src/engine/core-processor" \
  configure \
  clean \
  build \
  test \
  "--ci    For use with action $(builder_term test) - emits CI-friendly test reports"

builder_describe_outputs \
  configure     /node_modules \
  build         /web/build/engine/keyboard/lib/index.mjs

builder_parse "$@"

function do_configure() {
  verify_npm_setup

  # Configure Web browser-engine testing environments.  As is, this should only
  # make changes when we update the dependency, even on our CI build agents.
  playwright install
}

BUILD_DIR="${KEYMAN_ROOT}/web/build/${SUBPROJECT_NAME}"

do_build() {
  tsc --build "${THIS_SCRIPT_PATH}/tsconfig.all.json"

  # Base product - the main keyboard processor
  builder_echo "Bundle base product - the main keyboard processor"
  ${BUNDLE_CMD}  "${BUILD_DIR}/obj/index.js" \
    --out        "${BUILD_DIR}/lib/index.mjs" \
    --format esm

  # The DOM-oriented keyboard loader
  builder_echo "Bundle the DOM-oriented keyboard loader"
  ${BUNDLE_CMD}  "${BUILD_DIR}/obj/keyboards/loaders/dom-keyboard-loader.js" \
    --out        "${BUILD_DIR}/lib/dom-keyboard-loader.mjs" \
    --format esm

  # The Node-oriented keyboard loader
  builder_echo "Bundle the Node-oriented keyboard loader"
  ${BUNDLE_CMD}  "${BUILD_DIR}/obj/keyboards/loaders/node-keyboard-loader.js" \
    --out        "${BUILD_DIR}/lib/node-keyboard-loader.mjs" \
    --format   esm \
    --platform node

  # Declaration bundling.
  builder_echo "Declaration bundling"
  tsc --emitDeclarationOnly --outFile "${BUILD_DIR}/lib/index.d.ts"
  tsc --emitDeclarationOnly --outFile "${BUILD_DIR}/lib/dom-keyboard-loader.d.ts" -p src/keyboards/loaders/tsconfig.dom.json
  tsc --emitDeclarationOnly --outFile "${BUILD_DIR}/lib/node-keyboard-loader.d.ts" -p src/keyboards/loaders/tsconfig.node.json
}

do_test() {
  test-headless "${SUBPROJECT_NAME}" ""
  test-headless-typescript "${SUBPROJECT_NAME}"
}

builder_run_action configure  do_configure
builder_run_action clean      rm -rf "${BUILD_DIR}"
builder_run_action build      do_build
builder_run_action test       do_test
