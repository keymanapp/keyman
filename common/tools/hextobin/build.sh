#!/usr/bin/env bash
#
# Builds hextobin.js
#

# Exit on command failure and when using unset variables:
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

################################ Main script ################################

builder_describe "Build hextobin" clean configure build
builder_describe_outputs \
  configure /node_modules \
  build     build/index.js
builder_parse "$@"

if builder_start_action clean; then
  npm run clean
  builder_finish_action success clean
fi

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

if builder_start_action build; then
  npm run build
  builder_finish_action success build
fi

