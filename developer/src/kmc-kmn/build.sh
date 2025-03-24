#!/usr/bin/env bash
#
# Compiles the kmc-kmn keyboard compiler.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"

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
  "test" \
  "publish                   publish to npm" \
  "--npm-publish+            For publish, do a npm publish, not npm pack (only for CI)" \
  "--dry-run,-n              don't actually publish, just dry run"

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
  verify_npm_setup
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
  builder_do_typescript_tests 80
}

builder_run_action build      do_build
builder_run_action api        api-extractor run --local --verbose
builder_run_action test       do_test

#-------------------------------------------------------------------------------------------------------------------

builder_run_action publish  builder_publish_npm
