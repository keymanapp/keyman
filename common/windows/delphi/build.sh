#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
  "Build Keyman Developer common files" \
  clean configure build test \
  :tools :components :ext :keymanversion

builder_parse "$@"
builder_run_child_actions clean configure build test

function do_build() {
  "$KEYMAN_ROOT/common/windows/mkver.sh" "$KEYMAN_ROOT/common/windows/delphi/general/keymanversion_build.in" "$KEYMAN_ROOT/common/windows/delphi/general/keymanversion_build.inc"
  # $(GIT_BASH_FOR_KEYMAN) $(KEYMAN_ROOT)/common/include/build.sh clean configure build
}

builder_run_action build:keymanversion do_build
