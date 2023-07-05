#!/usr/bin/env bash
#
# Compile KeymanWeb's 'keyboard-processor' module, one of the components of Web's 'core' module.
#
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

################################ Main script ################################

builder_describe \
  "Compiles the web-oriented utility function module." \
  "@/common/web/recorder  test" \
  "@/common/web/keyman-version" \
  "@/common/web/es-bundling" \
  "@/common/web/utils" \
  configure \
  clean \
  build \
  test \
  "--ci    For use with action $(builder_term test) - emits CI-friendly test reports"

builder_describe_outputs \
  configure     /node_modules \
  build         /common/web/keyboard-processor/build/lib/index.mjs

builder_parse "$@"

function do_build() {
  tsc --build "$THIS_SCRIPT_PATH/tsconfig.all.json"
  node ./build-bundler.js

  # Declaration bundling.
  tsc --emitDeclarationOnly --outFile ./build/lib/index.d.ts
  tsc --emitDeclarationOnly --outFile ./build/lib/dom-keyboard-loader.d.ts -p src/keyboards/loaders/tsconfig.dom.json
  tsc --emitDeclarationOnly --outFile ./build/lib/node-keyboard-loader.d.ts -p src/keyboards/loaders/tsconfig.node.json
}

function do_test() {
  local MOCHA_FLAGS=
  local KARMA_CONFIG=manual.conf.cjs
  if builder_has_option --ci; then
    echo "Replacing user-friendly test reports with CI-friendly versions."
    MOCHA_FLAGS="$MOCHA_FLAGS --reporter mocha-teamcity-reporter"
    KARMA_CONFIG=CI.conf.cjs
  fi

  c8 mocha --recursive $MOCHA_FLAGS ./tests/node/
  karma start ./tests/dom/$KARMA_CONFIG
}

builder_run_action configure  verify_npm_setup
builder_run_action clean      rm -rf ./build
builder_run_action build      do_build
builder_run_action test       do_test