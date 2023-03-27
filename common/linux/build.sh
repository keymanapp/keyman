#!/usr/bin/env bash
#
# Compiles and tests the common Linux modules
#

# Exit on command failure and when using unset variables:
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

#
# TODO: when we have linux-specific tests, add them here
#       as child modules
#

builder_describe "Keyman common Linux modules" \
  clean \
  configure \
  build \
  test

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

builder_run_child_actions clean configure build test
