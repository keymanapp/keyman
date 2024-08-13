#!/usr/bin/env bash
#
# Compiles development-related KeymanWeb resources for use with developing/running tests.
#   - the Recorder module (for engine tests)
#   - the DOM module (for touch-alias and element-interface tests)

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

################################ Main script ################################


builder_describe \
  "Compiles the web-oriented utility function module." \
  "@/common/web/keyman-version" \
  configure \
  clean \
  build

builder_describe_outputs \
  configure   "/node_modules" \
  build       "/common/web/recorder/build/obj/index.js"

builder_parse "$@"

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

if builder_start_action clean; then
  tsc --build --clean "$THIS_SCRIPT_PATH/tsconfig.json"
  builder_finish_action success clean
fi

if builder_start_action build; then
  tsc --build "$THIS_SCRIPT_PATH/tsconfig.json"
  builder_finish_action success build
fi