#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
# END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
  "Tests child builds not run twice" \
  @dep build

function do_build() {
  echo "child: do_build"
}

builder_parse "$@"

builder_run_action build do_build

