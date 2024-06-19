#!/usr/bin/env bash
#
# Packages the @keymanapp/models-types package.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"

builder_describe "Keyman model types package" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "publish                   publish to npm" \
  "--npm-publish+            For publish, do a npm publish, not npm pack (only for CI)" \
  "--dry-run,-n              don't actually publish, just dry run"

builder_describe_outputs \
  configure     /node_modules \
  build         /common/models/types/build/common/models/types/tsconfig.tsbuildinfo

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean        rm -rf ./build/
builder_run_action configure    verify_npm_setup
builder_run_action build        tsc --build
builder_run_action test         tsc test.ts -lib es6
builder_run_action publish      builder_publish_npm
