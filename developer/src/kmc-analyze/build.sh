#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"

cd "$THIS_SCRIPT_PATH"

builder_describe "Build Keyman Developer Compiler Analysis Tools" \
  "@/common/web/types" \
  "@/developer/src/kmc-kmn" \
  clean configure build test publish pack \
  "--dry-run,-n              don't actually publish, just dry run"

builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/kmc-analyze/build/src/index.js

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action clean; then
  rm -rf ./build/
  builder_finish_action success clean
fi

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

if builder_start_action build; then
  tsc --build
  builder_finish_action success build
fi

if builder_start_action test; then
  eslint .
  # TODO: enable tests
  #     cd test && tsc --build && cd .. && mocha
  # TODO: enable c8 (disabled because no coverage at present)
  #     c8 --reporter=lcov --reporter=text mocha
  builder_finish_action success test
fi

if builder_start_action publish; then
  builder_publish_to_npm
  builder_finish_action success publish
fi

if builder_start_action pack; then
  builder_publish_to_pack
  builder_finish_action success pack
fi
