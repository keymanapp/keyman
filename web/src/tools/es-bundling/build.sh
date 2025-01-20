#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

################################ Main script ################################

builder_describe "Builds KMW's esbuild-oriented common configuration & tooling" \
  "clean" \
  "configure" \
  "build"

builder_describe_outputs \
  configure          /node_modules \
  build              /web/src/tools/es-bundling/build/index.mjs

builder_parse "$@"

builder_run_action configure  verify_npm_setup
builder_run_action clean      rm -rf build/
builder_run_action build      tsc -b tsconfig.json
