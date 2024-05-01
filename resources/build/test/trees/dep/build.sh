#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/build-utils.sh"
# END STANDARD BUILD SCRIPT INCLUDE

# Test builder_describe_outputs and dependencies

project=dep

builder_describe "$project test module" \
  clean \
  configure \
  build

builder_parse "$@"

if builder_is_child_build; then
  builder_die "FAIL: builder_is_child_build should return false but was $_builder_is_child for a dependency script"
else
  builder_echo "PASS: builder_is_child_build is false ($_builder_is_child) for the dependency script"
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
