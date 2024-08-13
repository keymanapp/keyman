#!/usr/bin/env bash
#
# Compile KeymanWeb's dev & test tool modules

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

################################ Main script ################################

builder_describe "Builds the sourcemap-sanitizing script used for Keyman Engine for Web builds" \
  "@/common/tools/sourcemap-path-remapper" \
  "clean" \
  "configure" \
  "build" \

builder_describe_outputs \
  configure                   /node_modules \
  build                       /web/build/tools/building/sourcemap-root/index.js

builder_parse "$@"

### CONFIGURE ACTIONS

builder_run_action configure  verify_npm_setup
builder_run_action clean      rm -rf ../../../../build/tools/building/sourcemap-root
builder_run_action build      tsc --build tsconfig.json