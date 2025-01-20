#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
  "Keyman for Windows automatic tests" \
  clean configure build test \
  \
  :androidstringtokeymanlocalestring \
  :group-helper-rsp19902 \
  :jsonutil \
  :shared-data \
  :standards-data \
  :windows-setup \
  :ui-language-manager

builder_parse "$@"

builder_run_child_actions clean configure build test

# TODO: move androidstringtokeymanlocalestring, jsonutil, standards-data to common/