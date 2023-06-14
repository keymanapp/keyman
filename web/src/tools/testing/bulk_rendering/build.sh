#! /usr/bin/env bash
#
# Compile the KeymanWeb bulk-renderer module for use with developing/running engine tests.

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

################################ Main script ################################

builder_describe \
  "@/web/src/app/browser build" \
  "@/web/src/app/ui build" \
  "Build the bulk renderer project. The bulk renderer loads all the cloud keyboards from api.keyman.com and renders each of them to a document." \
  "clean" \
  "configure     runs 'npm ci' on root folder" \
  "build         (default) builds bulk_renderer to web/build/tools/testing/bulk_rendering/"

builder_describe_outputs \
  configure  /node_modules \
  build      /web/build/tools/testing/bulk_rendering/lib/bulk_render.js

builder_parse "$@"

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

if builder_start_action clean; then
  rm -rf ../../../../build/tools/testing/bulk_rendering
  builder_finish_action success clean
fi

if builder_start_action build; then
  tsc --build "$THIS_SCRIPT_PATH/tsconfig.json" $builder_verbose
  node build-bundler.js
  builder_finish_action success build
fi
