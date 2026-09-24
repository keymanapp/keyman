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
. "$KEYMAN_ROOT/resources/build/ci/ci-publish.inc.sh"

builder_describe "Keyman kmc-convert keyboard conversion tools module" \
  "@/common/web/keyman-version" \
  "@/common/web/types" \
  "@/developer/src/common/web/utils" \
  "@/developer/src/common/web/test-helpers" \
  clean configure build api test

builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/kmc-convert/build/src/main.js \
  api           /developer/build/api/kmc-convert.api.json

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean       rm -rf ./build/ ./tsconfig.tsbuildinfo
builder_run_action configure   node_select_version_and_npm_ci
builder_run_action build       tsc --build
builder_run_action api         typescript_run_api_extractor developer/src/kmc-convert main.d.ts
builder_run_action test        typescript_run_eslint_mocha_tests
