#!/usr/bin/env bash
#
# Builds the include script for the current Keyman version.
#

# Exit on command failure and when using unset variables:
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$(dirname $THIS_SCRIPT)"

display_usage ( ) {
  echo "Usage: $0 [configure] [clean] [build] [test]"
  echo "          [--verbose|-v]"
  echo "       $0 -h|--help"
  echo
  echo "  clean                  removes build/ folder"
  echo "  build                  builds wrapped version of package"
  echo "                           [if required will: configure]"
}

build ( ) {
  npm run tsc -- -b "$(dirname $THIS_SCRIPT)/tsconfig.json"
}

################################ Main script ################################

builder_init "clean build" "$@"

# TODO: build if out-of-date if test is specified
# TODO: configure if npm has not been run, and build is specified

if builder_has_action clean; then
  npm run clean
  echo "Cleaning recorder page"
  ./recorder/build.sh clean
  builder_report clean success
fi

if builder_has_action build; then
  echo "Building recorder page"
  ./recorder/build.sh build
  echo "Building unit-test resources module"
  ./unit-test-resources/build.sh build
  builder_report build success
fi
