#!/usr/bin/env bash
# Copyright (C) 2025 SIL International
# Distributed under the MIT License. See LICENSE.md file in the project
# root for full license information.
#
# TC build script for Keyman Developer on Linux

# shellcheck disable=SC2164
# shellcheck disable=SC1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

################################ Main script ################################

builder_describe \
  "Build Keyman Developer on Linux" \
  "all            run all actions" \
  "build          build"

builder_parse "$@"

cd "${KEYMAN_ROOT}/developer/src"

function build_developer_action() {
  builder_echo start "build developer" "Building Keyman Developer"

  ./build.sh configure build test

  builder_echo end "build developer" success "Finished building Keyman Developer"
}

if builder_has_action all; then
  build_developer_action
else
  builder_run_action build build_developer_action
fi
