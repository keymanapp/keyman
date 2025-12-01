#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.

# shellcheck disable=SC2164
# shellcheck disable=SC1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE


################################ Main script ################################

builder_describe \
  "Test Keyman Core on WASM" \
  "all            full meson build and test of Keyman Core for WASM"

builder_parse "$@"

builder_run_action all  builder_launch /core/build.sh configure,build,test:wasm
