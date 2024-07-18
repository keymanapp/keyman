#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
# END STANDARD BUILD SCRIPT INCLUDE

# Test builder_describe_outputs and dependencies

project=child1

builder_describe "$project test module" \
  "@../dep" \
  clean \
  configure \
  build \
  test \
  install \
  error

builder_parse "$@"

if ! builder_is_child_build; then
  builder_die "FAIL: builder_is_child_build should return true but was $_builder_is_child for a child script"
else
  builder_echo "PASS: builder_is_child_build is true ($_builder_is_child) for the child script"
fi

function test_action() {
  local action=$1

  if builder_start_action $action; then
    touch ../$project.$action
    builder_finish_action success $action
  fi
}

test_action clean
test_action configure
test_action build
test_action test
test_action install

if builder_start_action error; then
  builder_die "This error action is supposed to die"
fi