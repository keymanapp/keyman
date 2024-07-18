#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
  "Build Keyman Developer common files" \
  clean configure build test \
  :keymanversion :tools :components :ext

builder_parse "$@"

builder_run_child_actions clean
builder_run_action        clean                  rm -f general/keymanversion_build.inc

builder_run_child_actions configure

builder_run_action        build:keymanversion    replaceVersionStrings_Mkver general/keymanversion_build.in general/keymanversion_build.inc
builder_run_child_actions build test
