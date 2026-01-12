#!/usr/bin/env bash
#
# Compiles the kmc lexical model package compiler.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"
. "$KEYMAN_ROOT/resources/build/typescript.inc.sh"

builder_describe "Build Keyman kmc Package Compiler module" \
  "@/common/web/keyman-version" \
  "@/developer/src/common/web/test-helpers" \
  "@/developer/src/common/web/utils" \
  "configure" \
  "build" \
  "api                       analyze API and prepare API documentation" \
  "clean" \
  "test"

builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/kmc-package/build/src/main.js \
  api           /developer/build/api/kmc-package.api.json

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean      rm -rf ./build/ ./tsconfig.tsbuildinfo
builder_run_action configure  node_select_version_and_npm_ci
builder_run_action build      tsc --build
builder_run_action api        api-extractor run --local --verbose
builder_run_action test       typescript_run_eslint_mocha_tests
