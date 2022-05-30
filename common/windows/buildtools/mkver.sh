#!/bin/bash

# Set sensible script defaults:
# set -e: Terminate script if a command returns an error
set -e
# set -u: Terminate script if an unset variable is used
set -u

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

display_usage() {
  echo "Usage: mkver.sh <file.in> <file.out>"
  exit 1
}

if [[ $# -lt 2 ]]; then
  display_usage
fi

replaceVersionStrings_Mkver "$1" "$2"
