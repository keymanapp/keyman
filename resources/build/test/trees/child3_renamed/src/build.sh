#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../../resources/build/build-utils.sh"
# END STANDARD BUILD SCRIPT INCLUDE

# Test builder_describe_outputs and dependencies

project=child3

builder_describe "$project test module" \
  clean \
  configure \
  build \
  test \
  install \
  error

builder_parse "$@"

function test_action() {
  local action=$1

  if builder_start_action $action; then
    touch ../../$project.$action
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