#!/usr/bin/env bash

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/build-utils.sh"
# END STANDARD BUILD SCRIPT INCLUDE

cd "$THIS_SCRIPT_PATH"

echo "--- Build a child script which has ignored options and flags ---"
./build.sh test1 test2 --option1 --option2
