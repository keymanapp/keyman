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

builder_describe "Build Keyman kmc-copy module" \
  "@/common/web/keyman-version" \
  "@/common/web/types" \
  "@/developer/src/common/web/test-helpers" \
  "@/developer/src/common/web/utils" \
  clean configure build api test publish \
  "--npm-publish+            For publish, do a npm publish, not npm pack (only for CI)" \
  "--dry-run,-n              don't actually publish, just dry run"

builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/kmc-copy/build/src/main.js \
  api           /developer/build/api/kmc-copy.api.json

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

do_test() {
  eslint .
  cd test
  tsc --build
  cd ..
  readonly C8_THRESHOLD=30
  c8 -skip-full --reporter=lcov --reporter=text --lines $C8_THRESHOLD --statements $C8_THRESHOLD --branches $C8_THRESHOLD --functions $C8_THRESHOLD mocha "${builder_extra_params[@]}"
  builder_echo warning "Coverage thresholds are currently $C8_THRESHOLD%, which is lower than ideal."
  builder_echo warning "Please increase threshold in build.sh as test coverage improves."
}

builder_run_action clean      rm -rf ./build/
builder_run_action configure  verify_npm_setup
builder_run_action build      tsc --build
builder_run_action api        api-extractor run --local --verbose
builder_run_action test       do_test
builder_run_action publish    builder_publish_npm
