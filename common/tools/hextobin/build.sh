#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

################################ Main script ################################

builder_describe "Build hextobin" clean configure build
builder_describe_outputs \
  configure /common/tools/hextobin/.configured \
  build     /common/tools/hextobin/build/index.js

builder_parse "$@"

builder_run_action clean      rm -rf build/ node_modules/
builder_run_action configure  verify_npm_setup
builder_run_action build      tsc --build
