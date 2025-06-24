#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.

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
  "all            full meson build and test of Keyman Core"

builder_parse "$@"

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

function do_all() {
  "${KEYMAN_ROOT}/core/build.sh" configure:${ARCH} build:${ARCH} test:${ARCH}
  if is_windows; then
    "${KEYMAN_ROOT}/core/build.sh" configure:x64 build:x64 test:x64
  fi
}

builder_run_action  all  do_all
