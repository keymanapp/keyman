#!/usr/bin/env bash
#
# Compile KeymanWeb's dev & test tool modules

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

SUBPROJECT_NAME=tools/testing/recorder
. "$KEYMAN_ROOT/web/common.inc.sh"
. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"

################################ Main script ################################

builder_describe "Builds the Keyman Engine for Web's test-sequence recording tool" \
  "@/common/tools/es-bundling  build" \
  "@/common/web/keyman-version build" \
  "@/web/src/engine            build" \
  "@../recorder-core           build" \
  "clean" \
  "configure" \
  "build"

builder_describe_outputs \
  configure  /node_modules \
  build      /web/build/$SUBPROJECT_NAME/lib/index.mjs

builder_parse "$@"

do_build ( ) {
  compile $SUBPROJECT_NAME

  # Base product - the main keyboard processor
  node_es_bundle "${KEYMAN_ROOT}/web/build/$SUBPROJECT_NAME/obj/index.js" \
    --out        "${KEYMAN_ROOT}/web/build/$SUBPROJECT_NAME/lib/index.mjs" \
    --format esm
}

builder_run_action configure node_select_version_and_npm_ci
builder_run_action clean rm -rf ../../../../build/$SUBPROJECT_NAME/
builder_run_action build do_build