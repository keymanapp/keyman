#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"

builder_describe "Build Keyman kmc-generate module" \
  "@/common/web/keyman-version" \
  "@/common/web/types" \
  "@/developer/src/common/web/test-helpers" \
  "configure" \
  "build" \
  "api                       analyze API and prepare API documentation" \
  "clean" \
  "test" \
  "pack                      build a local .tgz pack for testing" \
  "publish                   publish to npm" \
  "--dry-run,-n              don't actually publish, just dry run"
builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/kmc-generate/build/src/main.js \
  api           /developer/build/api/kmc-generate.api.json

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

do_build() {
  tsc --build
  rm -rf ./build/src/template
  mkdir -p ./build/src/template
  cp -R ./src/template/ ./build/src/
}

do_test() {
  eslint .
  cd test
  tsc -b
  cd ..
  c8 --reporter=lcov --reporter=text mocha "${builder_extra_params[@]}"
}

builder_run_action clean      rm -rf ./build/ ./tsconfig.tsbuildinfo
builder_run_action configure  verify_npm_setup
builder_run_action build      do_build
builder_run_action api        api-extractor run --local --verbose
builder_run_action test       do_test
builder_run_action publish    builder_publish_to_npm
builder_run_action pack       builder_publish_to_pack
