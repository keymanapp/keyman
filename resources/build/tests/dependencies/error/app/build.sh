#!/usr/bin/env bash

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../../resources/build/build-utils.sh"
# END STANDARD BUILD SCRIPT INCLUDE

cd "$THIS_SCRIPT_PATH"

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
