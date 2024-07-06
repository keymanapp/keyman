#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

#
# TODO: when we have windows-specific tests, add them here
#       as child modules
#

builder_describe "Keyman common Windows modules" \
  clean \
  configure \
  build \
  test

builder_parse "$@"

if [[ $BUILDER_OS != win ]]; then
  builder_echo grey "Platform is not Windows; skipping common/windows"
  exit 0
fi

#-------------------------------------------------------------------------------------------------------------------

builder_run_child_actions clean configure build test
