#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

builder_describe "Keyman kmc-convert keyboard conversion tools module" \
  "@/developer/src/common/web/utils" \
  "@/developer/src/common/web/test-helpers" \
  "configure" \
  "build" \
  "api                       analyze API and prepare API documentation" \
  "clean" \
  "test" \
  "publish                   publish to npm" \
  "--npm-publish+            For publish, do a npm publish, not npm pack (only for CI)" \
  "--dry-run,-n              don't actually publish, just dry run"

builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/kmc-convert/build/src/main.js \
  api           /developer/build/api/kmc-convert.api.json

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean       rm -rf ./build/ ./tsconfig.tsbuildinfo
builder_run_action configure   verify_npm_setup
builder_run_action build       tsc --build
builder_run_action api         api-extractor run --local --verbose

do_test() {
  eslint .
  cd test
  tsc -b
  cd ..
  readonly C8_THRESHOLD=20
  c8 -skip-full --reporter=lcov --reporter=text --lines $C8_THRESHOLD --statements $C8_THRESHOLD --branches $C8_THRESHOLD --functions $C8_THRESHOLD mocha "${builder_extra_params[@]}"
  builder_echo warning "Coverage thresholds are currently $C8_THRESHOLD%, which is lower than ideal."
  builder_echo warning "Please increase threshold in build.sh as test coverage improves."
}

builder_run_action test        do_test
builder_run_action publish     builder_publish_npm
