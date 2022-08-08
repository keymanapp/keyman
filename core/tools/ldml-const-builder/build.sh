#!/usr/bin/env bash
#
# Builds the include script for the current Keyman version.
#

# Exit on command failure and when using unset variables:
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$(dirname "$THIS_SCRIPT")"

################################ Main script ################################

builder_describe "Build and run the constant builder for LDML" configure clean build run
builder_parse "$@"

# TODO: build if out-of-date if test is specified
# TODO: configure if npm has not been run, and build is specified

if builder_has_action configure; then
#   verify_npm_setup
  builder_report success configure
fi

if builder_has_action clean; then
#   npm run clean
  rm -rf ../../include/ldml/build/
  # TODO: clean .h?
  builder_report success clean
fi

if builder_has_action build; then
  # Generate index.ts
  npx tsc -b ../../include/ldml/

  builder_report success build
fi

if builder_has_action run; then
  node ../../include/ldml/build/core/include/ldml/ldml-const-builder.js

  builder_report success run
fi
