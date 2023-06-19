#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

cd "$THIS_SCRIPT_PATH"

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"

builder_describe "Build Keyman kmc KMW Keyboard Compiler module" \
  "@/common/web/types" \
  "configure" \
  "build" \
  "clean" \
  "test" \
  "pack                      build a local .tgz pack for testing" \
  "publish                   publish to npm" \
  "--dry-run,-n              don't actually publish, just dry run"

builder_describe_outputs \
  configure     /node_modules \
  build         build/src/main.js

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

function copy_schema() {
  # We need the schema file at runtime and bundled, so always copy it for all
  # actions except `clean` and `configure`
  mkdir -p "$THIS_SCRIPT_PATH/build/src/"
  cp "$KEYMAN_ROOT/common/schemas/keyman-touch-layout/keyman-touch-layout.spec.json" "$THIS_SCRIPT_PATH/build/src/"
  cp "$KEYMAN_ROOT/common/schemas/kvks/kvks.schema.json" "$THIS_SCRIPT_PATH/build/src/"
}

function do_build() {
  copy_schema
  tsc --build
}

function do_test() {
  copy_schema
  eslint .
  cd test
  tsc --build
  cd ..
  # TODO: add c8 for coverage
  mocha
}

function do_publish() {
  copy_schema
  builder_publish_to_npm
}

function do_pack() {
  copy_schema
  builder_publish_to_pack
}

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean       rm -rf ./build/ ./tsconfig.tsbuildinfo
builder_run_action configure   verify_npm_setup
builder_run_action build       do_build
builder_run_action test        do_test
builder_run_action pack        do_pack
builder_run_action publish     do_publish
