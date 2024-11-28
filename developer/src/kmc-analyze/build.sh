#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"

builder_describe "Keyman Developer Compiler Analysis Tools" \
  "@/common/web/types" \
  "@/developer/src/kmc-kmn" \
  "@/developer/src/common/web/utils" \
  clean configure build api test publish \
  "--npm-publish+            For publish, do a npm publish, not npm pack (only for CI)" \
  "--dry-run,-n              don't actually publish, just dry run"

builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/kmc-analyze/build/src/index.js \
  api           /developer/build/api/kmc-analyze.api.json

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean      rm -rf ./build/
builder_run_action configure  verify_npm_setup
builder_run_action build      tsc --build
builder_run_action api        api-extractor run --local --verbose
builder_run_action test       builder_do_typescript_tests 70
builder_run_action publish    builder_publish_npm
