#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
# END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
  "Tests dependency builds debug flag" \
  configure build

function do_build() {
  # Test the debug flag
  builder_is_debug_build || builder_die "FAIL: dep: expecting builder_is_debug_build to be true"
  echo "PASS: dep: builder_is_debug_build is true"

  builder_has_option --debug || builder_die "FAIL: dep: expecting builder_has_option --debug to be true"
  echo "PASS: dep: builder_has_option --debug is true"
}

builder_parse "$@"

# Sanity Check: verify that we are running as a dep (and not a child) build
builder_is_dep_build || builder_die "FAIL: dep: builder_is_dep_build should be true"
! builder_is_child_build || builder_die "FAIL: dep: builder_is_child_build should be false"

builder_run_action build do_build
