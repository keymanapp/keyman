#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
# END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
  "Tests child builds debug flag" \
  @dep build

function do_build() {
  # Test the debug flag
  builder_is_debug_build || builder_die "FAIL: child: expecting builder_is_debug_build to be true"
  builder_echo green "  ✓ PASS: child: builder_is_debug_build is true"

  builder_has_option --debug || builder_die "FAIL: child: expecting builder_has_option --debug to be true"
  builder_echo green "  ✓ PASS: child: builder_has_option --debug is true"
}

builder_parse "$@"

# Sanity Check: verify that we are running as a child (and not a dep) build
! builder_is_dep_build || builder_die "FAIL: child: builder_is_dep_build should be false"
builder_is_child_build || builder_die "FAIL: child: builder_is_child_build should be true"

builder_run_action build do_build

