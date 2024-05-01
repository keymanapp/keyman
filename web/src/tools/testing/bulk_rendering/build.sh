#! /usr/bin/env bash
#
# Compile the KeymanWeb bulk-renderer module for use with developing/running engine tests.

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

SUBPROJECT_NAME=tools/testing/bulk_rendering
. "$KEYMAN_ROOT/web/common.inc.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

################################ Main script ################################

builder_describe "Builds a 'bulk renderer' that loads all the cloud keyboards from api.keyman.com and renders each of them to a document." \
  "@/web/src/app/browser build" \
  "@/web/src/app/ui build" \
  "clean" \
  "configure     runs 'npm ci' on root folder" \
  "build         (default) builds bulk_renderer to web/build/$SUBPROJECT_NAME/"

builder_describe_outputs \
  configure  /node_modules \
  build      /web/build/$SUBPROJECT_NAME/lib/bulk_render.js

builder_parse "$@"

function do_build ( ) {
  tsc --build "$THIS_SCRIPT_PATH/tsconfig.json" $builder_verbose

  $BUNDLE_CMD    "${KEYMAN_ROOT}/web/build/$SUBPROJECT_NAME/obj/renderer_core.js" \
    --out        "${KEYMAN_ROOT}/web/build/$SUBPROJECT_NAME/lib/bulk_render.js" \
    --sourceRoot "@keymanapp/keyman/web/build/$SUBPROJECT_NAME/lib/"
}

builder_run_action configure  verify_npm_setup
builder_run_action clean      rm -rf ../../../../build/$SUBPROJECT_NAME
builder_run_action build      do_build