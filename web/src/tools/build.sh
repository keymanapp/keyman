#!/usr/bin/env bash
#
# Compile KeymanWeb's dev & test tool modules
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

builder_describe "Builds the Keyman Engine for Web's development & unit-testing tools" \
  "@../../../common/web/keyman-version" \
  "@../../../common/web/keyboard-processor" \
  "@../../../common/web/recorder     :recorder" \
  "clean" \
  "configure" \
  "build" \
  ":recorder           Builds the KMW recorder submodule for development of unit-test resources" \

builder_describe_outputs \
  configure                   /node_modules \
  configure:recorder          /node_modules \
  build:recorder              testing/recorder/build/index.js

builder_parse "$@"

### CONFIGURE ACTIONS

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

### CLEAN ACTIONS

if builder_start_action clean:recorder; then
  rm -rf testing/recorder/build/
  builder_finish_action success clean:recorder
fi

### BUILD ACTIONS

if builder_start_action build:recorder; then
  npm run tsc -- -b src/tools/testing/recorder/tsconfig.json

  builder_finish_action success build:recorder
fi