#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"

builder_describe "Build Keyman Developer web utility module" \
  "@/common/web/types" \
  "clean" \
  "configure" \
  "build" \
  "api                       analyze API and prepare API documentation (no-op for now)" \
  "test" \
  publish \
  pack \
  "--dry-run,-n              don't actually publish, just dry run"

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

builder_run_action publish    builder_publish_to_npm
builder_run_action pack       builder_publish_to_pack
