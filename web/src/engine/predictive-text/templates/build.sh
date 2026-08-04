#!/usr/bin/env bash
#
# Compile our sourcemap-path remapping module for use by Web builds, releases, etc.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"
. "$KEYMAN_ROOT/web/common.inc.sh"

################################ Main script ################################

SUBPROJECT_NAME=engine/predictive-text/templates

builder_describe "Builds the predictive-text model template implementation module" \
  "@/web/src/common/web-utils build" \
  "@../wordbreakers           build" \
  "clean" \
  "configure" \
  "build" \
  "test"

builder_describe_outputs \
  configure          /node_modules \
  build              build/obj/index.js

builder_parse "$@"


function do_build() {
  tsc -b

  # Declaration bundling.
  tsc --emitDeclarationOnly --outFile ./build/lib/index.d.ts
}

builder_run_action clean      rm -rf build/
builder_run_action configure  node_select_version_and_npm_ci
builder_run_action build      do_build
builder_run_action test       test-headless-typescript $SUBPROJECT_NAME