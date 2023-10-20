#!/usr/bin/env bash
#
# Compiles the kmc-kmn keyboard compiler.
#

# Exit on command failure and when using unset variables:
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

cd "$THIS_SCRIPT_PATH"

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

builder_describe "Build Keyman Developer Compiler Module for .kmn to .kmx" \
  "@/common/web/keyman-version" \
  "@/common/web/types" \
  "@/developer/src/common/web/test-helpers" \
  "@/developer/src/kmcmplib:wasm" \
  "configure" \
  "build" \
  "clean" \
  "test" \
  "pack                      build a local .tgz pack for testing" \
  "publish                   publish to npm" \
  "--dry-run,-n              don't actually publish, just dry run"

builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/kmc-kmn/build/src/main.js

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

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action test; then
  copy_deps
  tsc --build test/
  npm run lint
  c8 --reporter=lcov --reporter=text --exclude-after-remap mocha "${builder_extra_params[@]}"
  builder_finish_action success test
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action publish; then
  . "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"
  builder_publish_to_npm
  builder_finish_action success publish
elif builder_start_action pack; then
  . "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"
  builder_publish_to_pack
  builder_finish_action success pack
fi
