#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Keyman -- all projects" \
  ":common" \
  ":core" \
  ":android" \
  ":ios" \
  ":linux" \
  ":mac" \
  ":web" \
  ":windows" \
  ":developer" \
  clean \
  configure \
  build \
  test \
  install \
  publish

builder_describe_platform \
  :android  android-studio \
  :ios      mac \
  :linux    linux \
  :mac      mac \
  :windows  win

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

builder_run_child_actions clean configure build test publish install
