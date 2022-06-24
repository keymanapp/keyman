#!/bin/bash
#
# Builds the include script for the current Keyman version.
#

# Exit on command failure and when using unset variables:
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$(dirname "$THIS_SCRIPT")"

display_usage ( ) {
  echo "Usage: $0 [configure] [clean] [build] [test]"
  echo "          [--verbose|-v]"
  echo "       $0 -h|--help"
  echo
  echo "  clean                  removes build/ folder"
  echo "  configure              runs 'npm ci' on root folder"
  echo "  build                  builds wrapped version of package"
  echo "                           [if required will: configure]"
  echo "  test                   runs tests (builds as req'd)"
  echo "                           [if required will: build]"
}

################################ Main script ################################

builder_init "clean configure build" "$@"

# TODO: build if out-of-date if test is specified
# TODO: configure if npm has not been run, and build is specified

if builder_has_action configure; then
  verify_npm_setup
  builder_report configure success
fi

if builder_has_action clean; then
  npm run clean
  rm -rf ../build
  builder_report clean success
fi

if builder_has_action build; then
  # Build
  npm run build -- $builder_verbose || die "Could not build @keymanapp/gesture-recognizer."
  builder_report build success
fi

if builder_has_action test; then
  npm test || fail "Tests failed"
  builder_report test success
fi
