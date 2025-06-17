#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# TC build script to test Keyman Core

# shellcheck disable=SC2164
# shellcheck disable=SC1091
# shellcheck disable=SC2248

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-helpers.inc.sh"

################################ Main script ################################

builder_describe \
  "Test Keyman Core" \
  "all            run all actions" \
  "build          full meson build and test of Keyman Core"

builder_parse "$@"

cd "${KEYMAN_ROOT}/core"

if is_ubuntu; then
  ARCH=arch
elif is_windows; then
  ARCH=x86
elif is_macos; then
  ARCH=mac
else
  builder_echo error "Unknown architecture"
  return 1
fi

if builder_has_action all; then
  "${KEYMAN_ROOT}/core/build.sh" configure:${ARCH} build:${ARCH} test:${ARCH}
else
  builder_run_action build  "${KEYMAN_ROOT}/core/build.sh" configure:${ARCH} build:${ARCH} test:${ARCH}
fi

if is_windows; then
  "${KEYMAN_ROOT}/core/build.sh" configure:x64 build:x64 test:x64
fi
