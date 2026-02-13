#!/usr/bin/env bash
#
# Compiles the kmc-kmn keyboard compiler.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"
. "$KEYMAN_ROOT/resources/build/typescript.inc.sh"

builder_describe "Keyman Developer Compiler Module for .kmn to .kmx" \
  "@/common/web/keyman-version" \
  "@/common/web/types" \
  "@/developer/src/common/web/test-helpers" \
  "@/developer/src/common/web/utils" \
  "@/developer/src/kmcmplib:wasm" \
  "configure" \
  "build" \
  "clean" \
  "api                       analyze API and prepare API documentation" \
  "test"

builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/kmc-kmn/build/src/main.js \
  api           /developer/build/api/kmc-kmn.api.json

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action clean; then
  rm -rf ./build/ ./tsconfig.tsbuildinfo
  builder_finish_action success clean
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action configure; then
  node_select_version_and_npm_ci
  builder_finish_action success configure
fi

#-------------------------------------------------------------------------------------------------------------------

function copy_deps() {
  mkdir -p build/src/import/kmcmplib
  mkdir -p src/import/kmcmplib
  cp ../kmcmplib/build/wasm/$BUILDER_CONFIGURATION/src/wasm-host.js ./src/import/kmcmplib/wasm-host.js
  cp ../kmcmplib/build/wasm/$BUILDER_CONFIGURATION/src/wasm-host.wasm ./build/src/import/kmcmplib/wasm-host.wasm
}

function do_build() {
  copy_deps
  tsc --build
}

function do_test() {
  copy_deps

  # We want to compare the key cap values from both KMW and Developer and ensure
  # that all three are in sync. We'll copy the relevant source files and patch
  # them in directly. The touch layout builder's constants.js is not an ES6
  # module, so we hackily patch that here.
  echo 'export const builder = {specialCharacters:{}}' > ./test/kmw/_imported_layoutbuilder_constants.js
  # shellcheck disable=SC2016
  echo 'function $(v) {v()}' >> ./test/kmw/_imported_layoutbuilder_constants.js
  cat "${KEYMAN_ROOT}/developer/src/tike/xml/layoutbuilder/constants.js" >> ./test/kmw/_imported_layoutbuilder_constants.js
  cp "${KEYMAN_ROOT}/web/src/engine/osk/src/specialCharacters.ts" ./test/kmw/_imported_web_osk_specialCharacters.ts

  typescript_run_eslint_mocha_tests 80
}

builder_run_action build      do_build
builder_run_action api        api-extractor run --local --verbose
builder_run_action test       do_test
