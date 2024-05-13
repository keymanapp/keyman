#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
# END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
  "Tests child and dependency builds --debug flag" \
  @dep :child build

function do_build() {
  builder_is_debug_build || builder_die "FAIL: parent: expecting builder_is_debug_build to be true"
  echo "PASS: parent: builder_is_debug_build is true"

  builder_has_option --debug || builder_die "FAIL: parent: expecting builder_has_option --debug to be true"
  echo "PASS: parent: builder_has_option --debug is true"
}

builder_parse build:child -d
builder_run_action build do_build
builder_run_child_actions build
