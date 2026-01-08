#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Build and test the pr-build-status.yml GHA" build test

builder_parse "$@"

YML_PREFIX_FILE="${THIS_SCRIPT_PATH}/pr-build-status.prefix.yml"
YML_FILE="${KEYMAN_ROOT}/.github/workflows/pr-build-status.yml"
MJS_FILE="${THIS_SCRIPT_PATH}/pr-build-status.mjs"

# Regenerate pr-build-status.yml from the script here
function do_build() {
  local YML_PREFIX=$(cat "$YML_PREFIX_FILE")
  local SCRIPT_CLONE=$(sed -n '/CLONE:START/,/CLONE:END/{//!p}' < "$MJS_FILE")
  local SCRIPT_SUFFIX=$(sed -n '/CLONE-COMMENTED:START/,/CLONE-COMMENTED:END/{//!p}' < "$MJS_FILE" | sed 's/\/\/ //g')
  echo $'# GENERATED FILE - DO NOT EDIT!\n'"$YML_PREFIX"$'\n'"$SCRIPT_CLONE"$'\n'"$SCRIPT_SUFFIX"$'\n' > "$YML_FILE"
}

builder_run_action build  do_build
builder_run_action test   npm test

