#!/usr/bin/env bash
#
# Compiles common TS-based utility functions for use among Keyman's codebase

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

cd "$THIS_SCRIPT_PATH"

################################ Main script ################################

builder_describe \
  "A ES5 + esbuild compatibility wrapper for the 'tslib' package." \
  clean configure build

builder_describe_outputs \
  configure "/node_modules" \
  build     "/common/web/tslib/build/index.js"

builder_parse "$@"

builder_run_action configure verify_npm_setup
builder_run_action clean rm -rf build/
builder_run_action build tsc --build "$THIS_SCRIPT_PATH/tsconfig.json"