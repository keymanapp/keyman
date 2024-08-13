#!/usr/bin/env bash
#
# Compiles the JS sourcemap remapper tool used by some of our TS projects when complications
# arise with sourcemap handling.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

################################ Main script ################################

builder_describe "Builds a sourcemap manipulation ES module for use in Web-related builds" \
  "clean" \
  "configure" \
  "build"

builder_describe_outputs \
  configure          /node_modules \
  build              build/index.js

builder_parse "$@"

### CLEAN ACTIONS

if builder_start_action clean; then
  rm -rf build/
  builder_finish_action success clean
fi

### CONFIGURE ACTIONS

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

### BUILD ACTIONS

if builder_start_action build; then
  npm run tsc

  builder_finish_action success build
fi