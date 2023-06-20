#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

################################ Main script ################################

builder_describe "Builds KMW's esbuild-oriented common configuration & tooling" \
  "@/common/web/tslib" \
  "clean" \
  "configure" \
  "build"

builder_describe_outputs \
  configure          /node_modules \
  build              /common/web/es-bundling/build/index.mjs

builder_parse "$@"

builder_run_action configure  verify_npm_setup
builder_run_action clean      rm -rf build/
builder_run_action build      tsc -b tsconfig.json