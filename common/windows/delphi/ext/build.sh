#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
  "Keyman common third party Delphi components" \
  clean configure build test reset \
  :cef4delphi :dcpcrypt :jwa :sentry :tds2dbg

builder_parse "$@"
builder_run_child_actions clean configure build test

function do_reset() {
  "$DEVTOOLS" -rp
  "$DEVTOOLS" -ri
}

builder_run_action reset do_reset
