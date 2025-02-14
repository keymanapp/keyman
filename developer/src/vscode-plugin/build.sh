#!/usr/bin/env bash
#
# Compiles the Keyman Developer for VSCode plugin.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

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

builder_run_action clean           rm -rf ./out/ ./tsconfig.tsbuildinfo .vscode-test ./build .parcel-cache
builder_run_action configure       verify_npm_setup
builder_run_action build           npm run compile
builder_run_action test            npm test

#-------------------------------------------------------------------------------------------------------------------

. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"
