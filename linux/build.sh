#!/usr/bin/env bash

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

################################ Main script ################################

cd "$THIS_SCRIPT_PATH"

OUTPUT_PATH=

builder_describe \
  "Build Keyman for Linux." \
  "clean" \
  "configure" \
  "build+" \
  "test" \
  ":engine=ibus-keyman       ibus-keyman" \
  "install                   install artifacts" \
  "uninstall                 uninstall artifacts"

builder_parse "$@"

builder_run_child_actions clean
builder_run_child_actions configure
builder_run_child_actions build
builder_run_child_actions test
builder_run_child_actions install
builder_run_child_actions uninstall
