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

builder_describe "Builds the gesture-recognition model for Web-based on-screen keyboards" \
  "clean" \
  "configure" \
  "build" \
  "tools" \
  "test"

builder_parse "$@"

# TODO: build if out-of-date if test is specified
# TODO: configure if npm has not been run, and build is specified

if builder_has_action configure; then
  verify_npm_setup
  builder_report success configure
fi

if builder_has_action clean; then
  npm run clean
  rm -rf build/
  builder_report success clean
fi

if builder_has_action build; then
  # Build
  npm run build -- $builder_verbose
  builder_report success build
fi

if builder_has_action tools; then
  src/tools/build.sh build
  builder_report success tools
fi

if builder_has_action test; then
  npm test
  builder_report success test
fi