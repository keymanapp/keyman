#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
# END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
  "Test that dependency/child builds are not run twice" \
  :child \
  configure build

function do_build() {
  echo "dep: do_build"
}

builder_parse "$@"

builder_echo "---------- Before builder_run_action"
builder_run_action        build do_build
builder_echo "---------- After builder_run_action"

builder_echo "---------- Before builder_run_child_actions"
builder_run_child_actions build
builder_echo "---------- After builder_run_child_actions"
