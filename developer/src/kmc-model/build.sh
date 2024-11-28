#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"

builder_describe "Keyman kmc Lexical Model Compiler module" \
  "@/common/web/keyman-version" \
  "@/developer/src/common/web/test-helpers" \
  "@/web/src/engine/predictive-text/templates/ test" \
  "clean" \
  "configure" \
  "build" \
  "api                       analyze API and prepare API documentation" \
  "test" \
  "publish                   publish to npm" \
  "--npm-publish+            For publish, do a npm publish, not npm pack (only for CI)" \
  "--dry-run,-n              don't actually publish, just dry run"

builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/kmc-model/build/src/main.js \
  api           /developer/build/api/kmc-model.api.json

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

function do_build() {
  tsc -b ./tools/tsconfig.json
  npm run build
}

function do_test() {
  local MOCHA_FLAGS=

  if [[ "${TEAMCITY_GIT_PATH:-}" != "" ]]; then
    # we're running in TeamCity
    MOCHA_FLAGS="-reporter mocha-teamcity-reporter"
  fi
  npm run lint
  cd test
  tsc -b
  cd ..
  c8 --reporter=lcov --reporter=text mocha ${MOCHA_FLAGS}
}

builder_run_action clean        rm -rf ./build/ ./tsconfig.tsbuildinfo
builder_run_action configure    verify_npm_setup
builder_run_action build        do_build
builder_run_action api          api-extractor run --local --verbose
builder_run_action test         do_test
builder_run_action publish      builder_publish_npm

