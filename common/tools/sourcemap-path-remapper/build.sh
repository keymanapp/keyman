#!/usr/bin/env bash
#
# Compile our sourcemap-path remapping module for use by Web builds, releases, etc.
#
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

################################ Main script ################################

# TODO: for predictive-text, we only need :headless, perhaps we should be splitting modules?
# TODO: remove :tools once kmlmc is a dependency for test:module

builder_describe "Builds a sourcemap manipulation ES module for use in Web-related builds" \
  "clean" \
  "configure" \
  "build"

builder_describe_outputs \
  configure          /node_modules \
  build              build/index.js

builder_parse "$@"

### CONFIGURE ACTIONS

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

### CLEAN ACTIONS

if builder_start_action clean; then
  rm -rf build/
  builder_finish_action success clean
fi

### BUILD ACTIONS

if builder_start_action build; then
  npm run tsc

  builder_finish_action success build
fi