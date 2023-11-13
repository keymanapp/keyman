#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

SUBPROJECT_NAME=engine/osk
. "$KEYMAN_ROOT/web/common.inc.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

# ################################ Main script ################################

builder_describe "Builds the Keyman Engine for Web's On-Screen Keyboard package (OSK)." \
  "@/common/web/input-processor build" \
  "@/web/src/engine/dom-utils build" \
  "@/web/src/engine/events build" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "--ci+                     Set to utilize CI-based test configurations & reporting."

# Possible TODO?s
# "upload-symbols   Uploads build product to Sentry for error report symbolification.  Only defined for $DOC_BUILD_EMBED_WEB" \

builder_describe_outputs \
  configure   /node_modules \
              /web/src/resources/osk/keymanweb-osk.ttf \
  build       /web/build/$SUBPROJECT_NAME/lib/index.mjs

builder_parse "$@"

#### Build action definitions ####

if builder_start_action configure; then
  verify_npm_setup
  cp "$KEYMAN_ROOT/common/resources/fonts/keymanweb-osk.ttf" "$KEYMAN_ROOT/web/src/resources/osk/"
  builder_finish_action success configure
fi

builder_run_action clean rm -rf "$KEYMAN_ROOT/web/build/$SUBPROJECT_NAME"
builder_run_action build compile $SUBPROJECT_NAME
builder_run_action test test-headless osk