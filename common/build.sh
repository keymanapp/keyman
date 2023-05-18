#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Keyman common and resources modules" \
  ":resources=../resources" \
  ":linux" \
  ":mac" \
  ":web" \
  ":windows" \
  clean \
  configure \
  build \
  test

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

builder_run_child_actions clean configure build test
