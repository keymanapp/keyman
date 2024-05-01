#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Keyman resources module" clean configure build test
builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

# clean, configure, and build are no-ops for resources/ folder; currently
# only have a test for builder.inc.sh

if builder_start_action test; then
  ./build/test/test.sh
  builder_finish_action success test
fi
