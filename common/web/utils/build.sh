#!/usr/bin/env bash
#
# Compiles common TS-based utility functions for use among Keyman's codebase
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

################################ Main script ################################

builder_describe \
  "Compiles the web-oriented utility function module." \
  configure clean build

builder_parse "$@"

if builder_has_action configure; then
  verify_npm_setup

  "$KEYMAN_ROOT/common/web/keyman-version/build.sh"

  builder_report success configure
fi

if builder_has_action clean; then
  npm run clean
  builder_report success clean
fi

if builder_has_action build; then
  npm run tsc -- --build "$THIS_SCRIPT_PATH/tsconfig.json"
  builder_report success build
fi