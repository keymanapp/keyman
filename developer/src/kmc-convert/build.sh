#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"
. "$KEYMAN_ROOT/resources/build/ci/ci-publish.inc.sh"

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

do_kmc_convert_test() {

  builder_echo heading "Creating keylayout.schema.JSON"
  "$KEYMAN_ROOT/resources/standards-data/keylayout/create_keylayout_schema.sh"

  builder_echo heading "Creating keylayout.schema.validator"
  "$KEYMAN_ROOT/common/web/types/build.sh" "configure"
}

builder_run_action clean       rm -rf ./build/ ./tsconfig.tsbuildinfo
builder_run_action configure   node_select_version_and_npm_ci
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

builder_run_action test        do_kmc_convert_test
builder_run_action test        do_test
builder_run_action publish     ci_publish_npm
