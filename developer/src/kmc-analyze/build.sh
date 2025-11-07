#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"
. "$KEYMAN_ROOT/resources/build/typescript.inc.sh"

builder_describe "Keyman Developer Compiler Analysis Tools" \
  "@/common/web/types" \
  "@/developer/src/kmc-kmn" \
  "@/developer/src/common/web/utils" \
  clean configure build api test

builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/kmc-analyze/build/src/index.js \
  api           /developer/build/api/kmc-analyze.api.json

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean      rm -rf ./build/
builder_run_action configure  node_select_version_and_npm_ci
builder_run_action build      tsc --build
builder_run_action api        api-extractor run --local --verbose
builder_run_action test       typescript_run_eslint_mocha_tests 75
