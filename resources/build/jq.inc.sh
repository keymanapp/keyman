# shellcheck shell=bash
#
# Setup JQ environment variable according to the user's system
#
# Windows: /resources/build/jq-win64.exe
# Linux/macOS: jq
#

if [[ -z "${JQ+x}" ]]; then
  ## START STANDARD BUILD SCRIPT INCLUDE
  # adjust relative paths as necessary
  JQ_THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
  # . "${THIS_SCRIPT%/*}/build-utils.sh"
  ## END STANDARD BUILD SCRIPT INCLUDE

  if builder_is_windows; then
    JQ="$(dirname "${JQ_THIS_SCRIPT}")/jq-win64.exe"
  else
    JQ=jq
  fi

  readonly JQ

  # JQ with inplace file replacement
  function jqi() {
    # shellcheck disable=2312
    cat <<< "$("${JQ}" -c "$1" < "$2")" > "$2"
  }
fi
