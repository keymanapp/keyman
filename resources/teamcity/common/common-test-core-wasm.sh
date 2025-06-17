#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script to test Keyman Core Desktop on WASM

# shellcheck disable=SC2164
# shellcheck disable=SC1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE


################################ Main script ################################

builder_describe \
  "Test Keyman Core on Linux" \
  "all            run all actions" \
  "build          full meson build and test of Keyman Core for WASM"

builder_parse "$@"

# shellcheck disable=SC2154
cd "${KEYMAN_ROOT}/core"

if builder_has_action all; then
  "${KEYMAN_ROOT}/core/build.sh" configure:wasm build:wasm test:wasm
else
  builder_run_action build  "${KEYMAN_ROOT}/core/build.sh" configure:wasm build:wasm test:wasm
fi
