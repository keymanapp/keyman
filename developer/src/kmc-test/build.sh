#!/usr/bin/env bash
#
# Keyman is copyright (C) SIL International. MIT License.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"

builder_describe "Build Keyman kmc-test module" \
  "@/common/web/keyman-version" \
  "@/common/web/types" \
  "@/developer/src/common/web/test-helpers" \
  "@/developer/src/common/web/utils" \
  "@/core:wasm" \
  clean configure build api test publish \
  "--npm-publish+            For publish, do a npm publish, not npm pack (only for CI)" \
  "--dry-run,-n              don't actually publish, just dry run"

builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/kmc-test/build/src/main.js \
  api           /developer/build/api/kmc-test.api.json

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

do_configure() {
  verify_npm_setup

  mkdir -p "src/import/core/"
  # we don't need this file for release builds, but it's nice to have
  # for reference and auto-completion
  cp "${KEYMAN_ROOT}/core/build/wasm/${BUILDER_CONFIGURATION}/src/keymancore.d.ts" "${THIS_SCRIPT_PATH}/src/import/core/"
}

copy_deps() {
  # TODO-KMC-TEST: we need to support both node and web versions of this. Why is there a node version at all?
  mkdir -p "${THIS_SCRIPT_PATH}/build/src/import/core/"
  cp "${KEYMAN_ROOT}/core/build/wasm/${BUILDER_CONFIGURATION}/src/keymancore.d.ts" "${THIS_SCRIPT_PATH}/build/src/import/core/"
  cp "${KEYMAN_ROOT}/core/build/wasm/${BUILDER_CONFIGURATION}/src/"km-core-node{.mjs,.wasm} "${THIS_SCRIPT_PATH}/build/src/import/core/"
  cp "${KEYMAN_ROOT}/core/build/wasm/${BUILDER_CONFIGURATION}/src/"km-core{.js,.wasm} "${THIS_SCRIPT_PATH}/build/src/import/core/"
}

do_build() {
  copy_deps
  tsc --build
}

builder_run_action clean      rm -rf ./build/ ./src/import/core/
builder_run_action configure  do_configure
builder_run_action build      do_build
builder_run_action api        api-extractor run --local --verbose

# note: `export TEST_SAVE_ARTIFACTS=1` to save a copy of artifacts to temp path
# note: `export TEST_SAVE_FIXTURES=1` to get a copy of cloud-based fixtures saved to online/
# TODO: -skip-full
builder_run_action test       builder_do_typescript_tests 40

builder_run_action publish    builder_publish_npm
