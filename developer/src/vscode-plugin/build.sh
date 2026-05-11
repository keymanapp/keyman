#!/usr/bin/env bash
#
# Compiles the Keyman Developer for VSCode plugin.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/node.inc.sh"

builder_describe "Keyman Developer for VSCode module" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "api" \
  "publish"

builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/vscode-plugin/out/extension.js

builder_parse "$@"

builder_run_action clean           rm -rf ./out/ ./tsconfig.tsbuildinfo .vscode-test
builder_run_action configure       node_select_version_and_npm_ci
builder_run_action build           npm run compile
builder_run_action test            npm test

#-------------------------------------------------------------------------------------------------------------------

