#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script to build and test common modules

# shellcheck disable=SC2164
# shellcheck disable=SC1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

################################ Main script ################################

builder_describe \
  "Run unit tests in /common" \
  "all            run all actions" \
  "build          build and test common modules"

builder_parse "$@"

# shellcheck disable=SC2154
cd "${KEYMAN_ROOT}/common"

if builder_has_action all; then
  "${KEYMAN_ROOT}/common/build.sh" configure build test
else
  builder_run_action build  "${KEYMAN_ROOT}/common/build.sh" configure build test
fi
