#!/bin/bash
#
# Setup JQ environment variable according to the user's system
#
# Windows: /resources/build/jq-win64.exe
# Linux/macOS: jq
#

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
# . "$(dirname "$THIS_SCRIPT")/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

case "${OSTYPE}" in
  "cygwin")
    JQ=$(dirname "$THIS_SCRIPT")/jq-win64.exe
    ;;
  "msys")
    JQ=$(dirname "$THIS_SCRIPT")/jq-win64.exe
    ;;
  *)
    JQ=jq
    ;;
esac

readonly JQ

# JQ with inplace file replacement
function jqi() {
  cat <<< "$($JQ -c "$1" < "$2")" > "$2"
}
