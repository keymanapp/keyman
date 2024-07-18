#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../../resources/build/builder.inc.sh"
# END STANDARD BUILD SCRIPT INCLUDE

# Test builder_describe_outputs and dependencies

builder_describe "app test module" \
  "@../error error" \
  error

builder_parse "$@"

if builder_start_action error:project; then
  echo " ... doing the 'error' action for 'app'; we shouldn't have gotten here"
  echo " ... because dependencies/error should have failed"
  exit 99
fi
