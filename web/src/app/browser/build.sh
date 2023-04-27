#!/usr/bin/env bash
#

# set -x
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

# Imports common Web build-script definitions & functions
SUBPROJECT_NAME=app/browser
. "$KEYMAN_ROOT/web/common.inc.sh"

# ################################ Main script ################################

builder_describe "Builds the Keyman Engine for Web's website-integrating version for use in non-puppeted browsers." \
  "@/common/web/input-processor build" \
  "@/web/src/engine/device-detect build" \
  "@/web/src/engine/paths build" \
  "@/web/src/engine/package-cache build" \
  "@/web/src/engine/events build" \
  "@/web/src/engine/osk build" \
  "@/web/src/engine/element-wrappers build" \
  "@/web/src/engine/main build" \
  "clean" \
  "configure" \
  "build" \
  "test"

# Possible TODO?s
# "upload-symbols   Uploads build product to Sentry for error report symbolification.  Only defined for $DOC_BUILD_EMBED_WEB" \

builder_parse "$@"

config=release
if builder_is_debug_build; then
  config=debug
fi

builder_describe_outputs \
  configure   /node_modules \
  build       /web/build/$SUBPROJECT_NAME/$config/keymanweb.js

#### Build action definitions ####

if builder_start_action configure; then
  verify_npm_setup

  builder_finish_action success configure
fi

if builder_start_action clean; then
  rm -rf "$KEYMAN_ROOT/web/build/$SUBPROJECT_NAME"
  builder_finish_action success clean
fi

if builder_start_action build; then
  compile $SUBPROJECT_NAME

  builder_finish_action success build
fi

if builder_start_action test; then
  # No headless tests of yet.
  builder_finish_action success test
fi