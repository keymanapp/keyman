#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"

builder_describe "Build Keyman Developer Compiler Analysis Tools" \
  "@/common/web/types" \
  "@/developer/src/kmc-kmn" \
  clean configure build api test publish pack \
  "--dry-run,-n              don't actually publish, just dry run"

builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/kmc-analyze/build/src/index.js \
  api           /developer/build/api/kmc-analyze.api.json

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

function do_test() {
  eslint .
  # TODO: enable tests
  #     cd test && tsc --build && cd .. && mocha
  # TODO: enable c8 (disabled because no coverage at present)
  #     c8 --reporter=lcov --reporter=text --exclude-after-remap mocha
}

builder_run_action clean      rm -rf ./build/
builder_run_action configure  verify_npm_setup
builder_run_action build      tsc --build
builder_run_action api        api-extractor run --local --verbose
builder_run_action test       do_test
builder_run_action publish    builder_publish_to_npm
builder_run_action pack       builder_publish_to_pack
