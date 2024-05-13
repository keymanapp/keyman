#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
# END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
"Tests interactions between child and dependency builds, in particular
trying to ensure that builds don't run twice" \
  :child \
  @child/dep \
  @dep \
  @dep/child \
  build

function do_build() {
  echo "do_build"
}

rm -f dep/child/actual-calls
rm -f child/dep/actual-calls

builder_parse build

builder_echo "---------- Before builder_run_action"
builder_run_action        build do_build
builder_echo "---------- After builder_run_action"

builder_echo "---------- Before builder_run_child_actions"
builder_run_child_actions build
builder_echo "---------- After builder_run_child_actions"


if ! diff dep/child/expected-calls dep/child/actual-calls; then
  builder_die "FAIL: Expected content of dep/child/actual-calls to match content of dep/child/expected-calls"
fi

if ! diff child/dep/expected-calls child/dep/actual-calls; then
  builder_die "FAIL: Expected content of child/dep/actual-calls to match content of child/dep/expected-calls"
fi