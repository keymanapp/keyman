#!/usr/bin/env bash
#
# Compiles the kmc lexical model package compiler.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"

builder_describe "Build Keyman kmc Package Compiler module" \
  "@/common/web/keyman-version" \
  "@/developer/src/common/web/test-helpers" \
  "@/developer/src/common/web/utils" \
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
  build         /developer/src/kmc-package/build/src/main.js \
  api           /developer/build/api/kmc-package.api.json

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean      rm -rf ./build/ ./tsconfig.tsbuildinfo
builder_run_action configure  verify_npm_setup
builder_run_action build      tsc --build
builder_run_action api        api-extractor run --local --verbose

if builder_start_action test; then
  eslint .
  cd test
  tsc --build
  cd ..
  c8 --reporter=lcov --reporter=text mocha
  builder_finish_action success test
fi

builder_run_action publish     builder_publish_npm
