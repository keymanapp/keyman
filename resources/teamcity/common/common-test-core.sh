#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.

# shellcheck disable=SC2164
# shellcheck disable=SC1091
# shellcheck disable=SC2248

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/teamcity/includes/tc-helpers.inc.sh"

################################ Main script ################################

builder_describe \
  "Test Keyman Core" \
  "all            full meson build and test of Keyman Core"

builder_parse "$@"

if builder_is_linux; then
  ARCH=arch
elif builder_is_windows; then
  ARCH=win
elif builder_is_macos; then
  ARCH=mac
else
  builder_echo error "Unknown architecture"
  exit 1
fi

function do_all() {
  builder_launch /core/tools/ldml-const-builder/build.sh clean,configure,build,run,test
  builder_launch /core/build.sh configure,build,test:${ARCH}
}

builder_run_action  all  do_all
