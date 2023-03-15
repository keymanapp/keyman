#!/usr/bin/env bash
#
# Compile KeymanWeb's 'keyboard-processor' module, one of the components of Web's 'core' module.
#
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

builder_describe \
  "Compiles the web-oriented utility function module." \
  "@/common/web/recorder  test" \
  "@/common/web/keyman-version" \
  "@/common/web/utils" \
  configure \
  clean \
  build \
  test \
  "--ci    For use with action $(builder_term test) - emits CI-friendly test reports"

builder_describe_outputs \
  configure     /node_modules \
  build         /common/web/keyboard-processor/build/index.js

builder_parse "$@"

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

if builder_start_action clean; then
  npm run clean
  builder_finish_action success clean
fi

if builder_start_action build; then
  tsc --build "$THIS_SCRIPT_PATH/src/tsconfig.json"
  builder_finish_action success build
fi

if builder_start_action test; then
  tsc --build "$THIS_SCRIPT_PATH/src/tsconfig.bundled.json"

  builder_heading "Running Keyboard Processor test suite"

  FLAGS=
  if builder_has_option --ci; then
    echo "Replacing user-friendly test reports with CI-friendly versions."
    FLAGS="$FLAGS --reporter mocha-teamcity-reporter"
  fi

  mocha --recursive $FLAGS ./tests/cases/

  builder_finish_action success test
fi