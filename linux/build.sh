#!/usr/bin/env bash

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

################################ Main script ################################

cd "$THIS_SCRIPT_PATH"

builder_describe \
  "Build Keyman for Linux." \
  ":engine=ibus-keyman       ibus-keyman" \
  ":config=keyman-config     keyman-config" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "install                   install artifacts" \
  "uninstall                 uninstall artifacts"

builder_parse "$@"

builder_run_child_actions clean configure build test install uninstall
