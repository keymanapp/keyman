#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
# END STANDARD BUILD SCRIPT INCLUDE

# Test builder_describe_outputs and dependencies

builder_describe "parent test module" \
  :child \
  test1 test2 \
  "--option1+   inheritable option" \
  "--option2+   second inheritable option"

builder_parse "$@"

builder_run_child_actions test1 test2

echo Done
