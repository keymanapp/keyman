#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
  "Keyman Developer common files" \
  clean configure build test \
  ":delphi                      Delphi components" \
  ":web                         Web components"

builder_describe_platform \
  :delphi win,delphi

builder_parse "$@"
builder_run_child_actions clean configure build test
