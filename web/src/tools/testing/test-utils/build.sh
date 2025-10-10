#!/usr/bin/env bash
#
# Compile KeymanWeb's automated js/ts test utilities

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

SUBPROJECT_NAME=tools/testing/test-utils
. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"
. "$KEYMAN_ROOT/web/common.inc.sh"

################################ Main script ################################

builder_describe "Automated js/ts test utilities for KeymanWeb" \
  "clean" \
  "configure" \
  "build"

builder_describe_outputs \
  configure  /node_modules \
  build      "/web/build/${SUBPROJECT_NAME}/obj/index.js"

builder_parse "$@"

do_build ( ) {
  compile "${SUBPROJECT_NAME}"
}

builder_run_action configure  node_select_version_and_npm_ci
builder_run_action clean      rm -rf "../../../../build/${SUBPROJECT_NAME}/"
builder_run_action build      do_build