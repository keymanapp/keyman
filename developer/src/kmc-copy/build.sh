#!/usr/bin/env bash
#
# Keyman is copyright (C) SIL International. MIT License.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"
. "$KEYMAN_ROOT/resources/build/typescript.inc.sh"

builder_describe "Build Keyman kmc-copy module" \
  "@/common/web/keyman-version" \
  "@/common/web/types" \
  "@/developer/src/common/web/test-helpers" \
  "@/developer/src/common/web/utils" \
  clean configure build api test

builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/kmc-copy/build/src/main.js \
  api           /developer/build/api/kmc-copy.api.json

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------



builder_run_action clean      rm -rf ./build/
builder_run_action configure  node_select_version_and_npm_ci
builder_run_action build      tsc --build
builder_run_action api        api-extractor run --local --verbose

# note: `export TEST_SAVE_ARTIFACTS=1` to save a copy of artifacts to temp path
# note: `export TEST_SAVE_FIXTURES=1` to get a copy of cloud-based fixtures saved to online/
# TODO: -skip-full
builder_run_action test       typescript_run_eslint_mocha_tests 75
