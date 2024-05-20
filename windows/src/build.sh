#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
  "Keyman for Windows" \
  \
  clean \
  configure \
  build \
  test \
  "publish                 Prepare files for distribution, publish symbols, and build installer module" \
  "install                 Install built programs locally" \
  \
  ":engine                 Keyman Engine for Windows" \
  ":desktop                Keyman for Windows" \
  ":components=global/delphi   Delphi components" \
  ":test=test/unit-tests   Shared unit tests"

builder_parse "$@"

builder_run_child_actions  clean configure build test publish install
