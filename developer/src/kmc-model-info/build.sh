#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

builder_describe "Build Keyman kmc Lexical Model model-info Compiler module" \
  "@/common/web/types" \
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
  build         /developer/src/kmc-model-info/build/src/model-info-compiler.js \
  api           /developer/build/api/kmc-model-info.api.json

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean        rm -rf ./build/ ./tsconfig.tsbuildinfo
builder_run_action configure    verify_npm_setup
builder_run_action build        tsc --build
builder_run_action api          api-extractor run --local --verbose
builder_run_action test         builder_do_typescript_tests 80

#-------------------------------------------------------------------------------------------------------------------

. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"

builder_run_action publish     builder_publish_npm
