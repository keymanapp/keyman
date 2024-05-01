#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

cd "$THIS_SCRIPT_PATH"

builder_describe \
  "Build Keyman Developer common files" \
  clean configure build api test publish install \
  ":delphi                      Delphi components" \
  ":web                         Web components"

builder_parse "$@"
builder_run_child_actions clean configure build test

# Note: 'api', 'publish', 'install' actions are defined here for commonality with
# sibling projects but are no-ops
