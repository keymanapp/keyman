#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"

################################ Main script ################################

builder_describe "esbuild style bundling tooling for web and common/web" \
  "clean" \
  "configure" \
  "build"

builder_describe_outputs \
  configure          /node_modules \
  build              /common/tools/es-bundling/build/index.mjs

builder_parse "$@"

builder_run_action configure  node_select_version_and_npm_ci
builder_run_action clean      rm -rf build/
builder_run_action build      tsc -b
