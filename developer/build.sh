#!/usr/bin/env bash
## START STANDARD UTILITY SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/build-utils.sh"
## END STANDARD UTILITY SCRIPT INCLUDE

# This is not a builder script but calls a builder script
"$THIS_SCRIPT_PATH/src/build.sh" "$@"
