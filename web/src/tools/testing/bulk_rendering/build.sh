#! /usr/bin/env bash
#
# Compile the KeymanWeb bulk-renderer module for use with developing/running engine tests.

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../../../resources/build/build-utils.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE

set_keyman_standard_build_path

################################ Main script ################################

builder_describe \
  "Build the bulk renderer project. The bulk renderer loads all the cloud keyboards from api.keyman.com and renders each of them to a document." \
  "clean" \
  "configure     runs 'npm ci' on root folder" \
  "build         (default) builds bulk_renderer to web/build/tools/testing/bulk_rendering/"
builder_parse "$@"

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

if builder_start_action clean; then
  rm -rf ../../../../build/tools/testing/bulk_rendering/
  builder_finish_action success clean
fi

if builder_start_action build; then
  tsc --build "$THIS_SCRIPT_PATH/tsconfig.json" $builder_verbose
  builder_finish_action success build
fi
