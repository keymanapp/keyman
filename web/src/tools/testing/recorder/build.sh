#!/usr/bin/env bash
#
# Compile KeymanWeb's dev & test tool modules
#
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

SUBPROJECT_NAME=tools/testing/recorder
. "$KEYMAN_ROOT/web/common.inc.sh"

################################ Main script ################################

builder_describe "Builds the Keyman Engine for Web's test-sequence recording tool" \
  "@/common/web/keyman-version" \
  "@/common/web/keyboard-processor" \
  "@/common/web/recorder" \
  "clean" \
  "configure" \
  "build"

builder_describe_outputs \
  configure  /node_modules \
  build      /web/build/$SUBPROJECT_NAME/lib/index.mjs

builder_parse "$@"

### CONFIGURE ACTIONS

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

### CLEAN ACTIONS

if builder_start_action clean; then
  rm -rf ../../../../build/$SUBPROJECT_NAME/
  builder_finish_action success clean
fi

### BUILD ACTIONS

if builder_start_action build; then
  compile $SUBPROJECT_NAME

  builder_finish_action success build
fi