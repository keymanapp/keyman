#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
  "Keyman Developer automatic tests" \
  clean configure build test \
  :compile-supplementary-support \
  :keyboard-js-info \
  :kmcomp \
  :kmx-file-languages \
  :model-ts-parser \
  :package-info

builder_parse "$@"

builder_run_child_actions clean configure build test
