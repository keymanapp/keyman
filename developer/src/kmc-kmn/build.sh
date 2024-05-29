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

if builder_start_action build; then
  copy_deps
  tsc --build
  builder_finish_action success build
fi

builder_run_action api        api-extractor run --local --verbose

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action test; then
  copy_deps
  tsc --build test/
  npm run lint
  readonly C8_THRESHOLD=74
  c8 --reporter=lcov --reporter=text --lines $C8_THRESHOLD --statements $C8_THRESHOLD --branches $C8_THRESHOLD --functions $C8_THRESHOLD mocha
  builder_echo warning "Coverage thresholds are currently $C8_THRESHOLD%, which is lower than ideal."
  builder_echo warning "Please increase threshold in build.sh as test coverage improves."

  builder_finish_action success test
fi

#-------------------------------------------------------------------------------------------------------------------

builder_run_action publish  builder_publish_npm
