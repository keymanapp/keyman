#!/usr/bin/env bash
#
# Compile KeymanWeb's dev & test tool modules
#
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

################################ Main script ################################

builder_describe "Builds the Keyman Engine for Web's test-sequence recording tool" \
  "@../../../../../common/web/keyman-version" \
  "@../../../../../common/web/keyboard-processor" \
  "@../../../../../common/web/recorder" \
  "clean" \
  "configure" \
  "build"

builder_describe_outputs \
  configure  /node_modules \
  build      ../../../../build/tools/testing/recorder/index.js

builder_parse "$@"

### CONFIGURE ACTIONS

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

### CLEAN ACTIONS

if builder_start_action clean; then
  rm -rf ../../../../build/tools/testing/recorder/
  builder_finish_action success clean
fi

### BUILD ACTIONS

if builder_start_action build; then
  npm run tsc -- -b $THIS_SCRIPT_PATH/tsconfig.json

  builder_finish_action success build
fi