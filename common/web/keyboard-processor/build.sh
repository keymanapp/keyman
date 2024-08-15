#!/usr/bin/env bash
#
# Compile KeymanWeb's 'keyboard-processor' module, one of the components of Web's 'core' module.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "${KEYMAN_ROOT}/resources/shellHelperFunctions.sh"

BUNDLE_CMD="node ${KEYMAN_ROOT}/common/web/es-bundling/build/common-bundle.mjs"

################################ Main script ################################

builder_describe \
  "Compiles the web-oriented utility function module." \
  "@/web/src/tools/testing/recorder-core  test" \
  "@/common/web/keyman-version" \
  "@/common/web/es-bundling" \
  "@/common/web/types" \
  "@/common/web/utils" \
  configure \
  clean \
  build \
  test \
  "--ci    For use with action $(builder_term test) - emits CI-friendly test reports" \
  "--remote    Uses a BrowserStack-based test configuration for legacy devices"

builder_describe_outputs \
  configure     /node_modules \
  build         /common/web/keyboard-processor/build/lib/index.mjs

builder_parse "$@"

function do_configure() {
  verify_npm_setup

  # Configure Web browser-engine testing environments.  As is, this should only
  # make changes when we update the dependency, even on our CI build agents.
  playwright install
}

function do_build() {
  tsc --build "${THIS_SCRIPT_PATH}/tsconfig.all.json"

  # Base product - the main keyboard processor
  builder_echo "Bundle base product - the main keyboard processor"
  ${BUNDLE_CMD}  "${KEYMAN_ROOT}/common/web/keyboard-processor/build/obj/index.js" \
    --out        "${KEYMAN_ROOT}/common/web/keyboard-processor/build/lib/index.mjs" \
    --format esm

  # The DOM-oriented keyboard loader
  builder_echo "Bundle the DOM-oriented keyboard loader"
  ${BUNDLE_CMD}  "${KEYMAN_ROOT}/common/web/keyboard-processor/build/obj/keyboards/loaders/dom-keyboard-loader.js" \
    --out        "${KEYMAN_ROOT}/common/web/keyboard-processor/build/lib/dom-keyboard-loader.mjs" \
    --format esm

  # The Node-oriented keyboard loader
  builder_echo "Bundle the Node-oriented keyboard loader"
  ${BUNDLE_CMD}  "${KEYMAN_ROOT}/common/web/keyboard-processor/build/obj/keyboards/loaders/node-keyboard-loader.js" \
    --out        "${KEYMAN_ROOT}/common/web/keyboard-processor/build/lib/node-keyboard-loader.mjs" \
    --format   esm \
    --platform node

  # Tests
  builder_echo "Bundle tests"
  ${BUNDLE_CMD} "${KEYMAN_ROOT}/common/web/keyboard-processor/build/tests/dom/cases/domKeyboardLoader.spec.js" \
    --out       "${KEYMAN_ROOT}/common/web/keyboard-processor/build/tests/dom/domKeyboardLoader.spec.mjs" \
    --format   esm

  # Declaration bundling.
  builder_echo "Declaration bundling"
  tsc --emitDeclarationOnly --outFile ./build/lib/index.d.ts
  tsc --emitDeclarationOnly --outFile ./build/lib/dom-keyboard-loader.d.ts -p src/keyboards/loaders/tsconfig.dom.json
  tsc --emitDeclarationOnly --outFile ./build/lib/node-keyboard-loader.d.ts -p src/keyboards/loaders/tsconfig.node.json
}

function do_test() {
  local MOCHA_FLAGS=
  local WTR_CONFIG=
  if builder_has_option --ci; then
    echo "Replacing user-friendly test reports with CI-friendly versions."
    MOCHA_FLAGS="$MOCHA_FLAGS --reporter mocha-teamcity-reporter"
    WTR_CONFIG=.CI
  elif builder_has_option --remote; then
    WTR_CONFIG=.remote
  fi

  c8 mocha --recursive $MOCHA_FLAGS ./tests/node/
  web-test-runner --config tests/dom/web-test-runner${WTR_CONFIG}.config.mjs
}

builder_run_action configure  do_configure
builder_run_action clean      rm -rf ./build
builder_run_action build      do_build
builder_run_action test       do_test
