#!/usr/bin/env bash
#
# Compiles the JS sourcemap remapper tool used by some of our TS projects when complications
# arise with sourcemap handling.
#

# Exit on command failure and when using unset variables:
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$(dirname "$THIS_SCRIPT")"

################################ Main script ################################

builder_describe \
  "Compiles the sourcemap-remapping compilation tool used by our predictive text and Web engine builds." \
  configure clean build

builder_describe_outputs \
  configure     /node_modules \
  build         build/index.js

builder_parse "$@"

### CONFIGURE ACTIONS

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

if builder_start_action clean; then
  rm -rf build/
  builder_finish_action success clean
fi

if builder_start_action build; then
  npm run tsc

  builder_finish_action success build
fi