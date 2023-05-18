#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"

cd "$THIS_SCRIPT_PATH"

# TODO:   "@/common/models/types" \

builder_describe "Build Keyman kmc Lexical Model Compiler module" \
  "@/common/web/keyman-version" \
  "@/developer/src/common/web/test-helpers" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "pack                      build a local .tgz pack for testing" \
  "publish                   publish to npm" \
  "--dry-run,-n              don't actually publish, just dry run"

builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/kmc-model/build/src/main.js

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

function do_build() {
  mkdir -p build/cjs-src
  npm run build
}

builder_run_action clean        rm -rf ./build/ ./tsconfig.tsbuildinfo
builder_run_action configure    verify_npm_setup
builder_run_action build        do_build
builder_run_action test         npm test
builder_run_action publish      builder_publish_to_npm
builder_run_action pack         builder_publish_to_pack

