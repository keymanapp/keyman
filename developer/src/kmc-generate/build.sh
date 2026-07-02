#!/usr/bin/env bash
#
# Keyman is copyright (C) SIL International. MIT License.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"
. "$KEYMAN_ROOT/resources/build/typescript.inc.sh"

builder_describe "Build Keyman kmc-generate module" \
  "@/common/web/keyman-version" \
  "@/common/web/langtags" \
  "@/common/web/types" \
  "@/developer/src/common/web/test-helpers" \
  "@/developer/src/common/web/utils" \
  clean configure build api test

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

builder_run_action clean      rm -rf ./build/ ./tsconfig.tsbuildinfo
builder_run_action configure  node_select_version_and_npm_ci
builder_run_action build      do_build
builder_run_action api        api-extractor run --local --verbose
builder_run_action test       typescript_run_eslint_mocha_tests
