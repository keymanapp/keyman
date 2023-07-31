#!/usr/bin/env bash

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
# END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
  "Error-generating delegate script" \
  "raise-build-error+" \
  "unreported-action"

builder_parse "$@"

if builder_start_action raise-build-error; then
  echo "Emulating build script error => failure report"
  exit 1
fi

if builder_start_action unreported-action; then
  echo "Leaving action 'success' unreported; should trigger trap message"
fi