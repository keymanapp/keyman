#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../../resources/build/build-utils.sh"
# END STANDARD BUILD SCRIPT INCLUDE

# Test builder_describe_outputs and dependencies

builder_describe "library test module" \
  configure \
  build

builder_parse "$@"

builder_describe_outputs \
  configure:project out.configure \
  build:project     out.build

if builder_start_action configure:project; then
  echo " ... doing the 'configure' action for 'library'"
  exit 22
fi

if builder_start_action build:project; then
  echo " ... doing the 'build' action for 'library'"
  exit 22
fi
