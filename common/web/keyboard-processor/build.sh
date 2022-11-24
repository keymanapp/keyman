#!/usr/bin/env bash
#
# Compile KeymanWeb's 'keyboard-processor' module, one of the components of Web's 'core' module.
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

# Ensures color var use in `builder_describe`'s argument respects the specified
# --color/--no-color option.
builder_check_color "$@"

builder_describe \
  "Compiles the web-oriented utility function module." \
  "@../recorder  test" \
  "@../keyman-version" \
  "@../utils" \
  configure \
  clean \
  build \
  test \
  "--ci    For use with action ${BUILDER_TERM_START}test${BUILDER_TERM_END} - emits CI-friendly test reports"

builder_describe_outputs \
  configure     /node_modules \
  build         build/index.js

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
  npm run tsc -- --build "$THIS_SCRIPT_PATH/tsconfig.json"
  builder_finish_action success build
fi

if builder_start_action test; then
  echo_heading "Running Keyboard Processor test suite"

  FLAGS=
  if builder_has_option --ci; then
    echo "Replacing user-friendly test reports with CI-friendly versions."
    FLAGS="$FLAGS --reporter mocha-teamcity-reporter"
  fi

  npm run mocha -- --recursive $FLAGS ./tests/cases/

  builder_finish_action success test
fi