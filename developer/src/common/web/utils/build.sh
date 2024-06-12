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
  "--npm-publish+            For publish, do a npm publish, not npm pack (only for CI)" \
  "--dry-run,-n              don't actually publish, just dry run"

builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/common/web/utils/build/src/index.js

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean       rm -rf ./build/
builder_run_action configure   verify_npm_setup
builder_run_action build       tsc --build

if builder_start_action test; then
  eslint .
  tsc --build test
  readonly C8_THRESHOLD=70
  c8 --reporter=lcov --reporter=text --exclude-after-remap --lines $C8_THRESHOLD --statements $C8_THRESHOLD --branches $C8_THRESHOLD --functions $C8_THRESHOLD mocha
  builder_echo warning "Coverage thresholds are currently $C8_THRESHOLD%, which is lower than ideal."
  builder_echo warning "Please increase threshold in build.sh as test coverage improves."
  builder_finish_action success test
fi

builder_run_action publish    builder_publish_npm
