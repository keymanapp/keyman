#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"
. "$KEYMAN_ROOT/resources/build/typescript.inc.sh"

builder_describe "Keyman kmc Lexical Model Compiler module" \
  "@/common/web/keyman-version" \
  "@/developer/src/common/web/test-helpers" \
  "@/web/src/engine/predictive-text/templates/ test" \
  "clean" \
  "configure" \
  "build" \
  "api                       analyze API and prepare API documentation" \
  "test"

builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/kmc-model/build/src/main.js \
  api           /developer/build/api/kmc-model.api.json

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

function do_build() {
  tsc -b ./tools/tsconfig.json
  tsc -b
}

builder_run_action clean        rm -rf ./build/ ./tsconfig.tsbuildinfo
builder_run_action configure    node_select_version_and_npm_ci
builder_run_action build        do_build
builder_run_action api          api-extractor run --local --verbose
builder_run_action test         typescript_run_eslint_mocha_tests
