#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

cd "$THIS_SCRIPT_PATH"

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

builder_describe "Build Keyman Developer web utility module" \
  "@/common/web/types" \
  "clean" \
  "configure" \
  "build" \
  "test"

builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/common/web/utils/build/index.js

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean       rm -rf ./build/
builder_run_action configure   verify_npm_setup
builder_run_action build       tsc --build

if builder_start_action test; then
  eslint .
  tsc --build test
  c8 --reporter=lcov --reporter=text --exclude-after-remap mocha
  builder_finish_action success test
fi
