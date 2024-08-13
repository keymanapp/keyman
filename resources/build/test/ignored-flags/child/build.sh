#!/usr/bin/env bash

echo "Params: $@"

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
# END STANDARD BUILD SCRIPT INCLUDE

# Test missing actions and options

project=child1

builder_describe "$project test module" test1 --option1

builder_parse "$@"

builder_run_action test1 echo "test1 action ran"
