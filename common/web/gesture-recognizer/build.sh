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
  "test" \
  ":module" \
  ":tools  tools for testing & developing test resources for this module" \
  "--ci    sets the --ci option for child scripts (i.e, the `test` action)"

builder_parse "$@"

# TODO: build if out-of-date if test is specified
# TODO: configure if npm has not been run, and build is specified

if builder_has_action configure :module; then
  verify_npm_setup
  builder_report success configure :module
fi

if builder_has_action clean :tools; then
  src/tools/build.sh clean
  builder_report success clean :tools
fi

if builder_has_action clean :module; then
  npm run clean
  rm -rf build/
  builder_report success clean :module
fi

if builder_has_action build :module; then
  # Build
  npm run build -- $builder_verbose
  builder_report success build :module
fi

if builder_has_action build :tools; then
  src/tools/build.sh build
  builder_report success build :tools
fi

if builder_has_action test :module; then
  if builder_has_option --ci; then
    npm test -- --ci
  else
    npm test
  fi
  builder_report success test :module
fi