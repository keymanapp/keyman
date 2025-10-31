#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"

# ################################ Main script ################################

builder_describe "Builds the Keyman Engine for Web's sample page setups." \
  "@/web/src/engine/sentry-manager build" \
  "@/web/src/app/browser build" \
  "@/web/src/app/ui build" \
  "@/web/src/tools build" \
  "configure           Does nothing for this project" \
  "clean" \
  "build" \
  "test                Does nothing for this project"

# Possible TODO?s
# "upload-symbols   Uploads build product to Sentry for error report symbolification.  Only defined for $DOC_BUILD_EMBED_WEB" \

builder_parse "$@"

DEST="web/build/test-resources"

builder_describe_outputs \
  build       "/$DEST/sentry-manager.js"

#### Resource paths ####

SENTRY_MANAGER_SRC="$KEYMAN_ROOT/web/src/engine/sentry-manager/build/lib/index.js"
SENTRY_MANAGER_MAP="$KEYMAN_ROOT/web/src/engine/sentry-manager/build/lib/index.js.map"

#### Build action definitions ####

GESTURE_PROCESSOR_BUILD="$KEYMAN_ROOT/web/src/engine/osk/gesture-processor/build/lib/."
GESTURE_PROCESSOR_TARGET="$KEYMAN_ROOT/web/build/engine/gesture-processor/lib/"

function do_copy() {
  mkdir -p "$KEYMAN_ROOT/$DEST/keyboards"

  # The next four lines are needed for the sentry-integration manual test page.
  cp "$SENTRY_MANAGER_SRC"  "$KEYMAN_ROOT/$DEST/sentry-manager.js"
  cp "$SENTRY_MANAGER_MAP"  "$KEYMAN_ROOT/$DEST/sentry-manager.js.map"

  mkdir -p "$GESTURE_PROCESSOR_TARGET"
  cp -a "$GESTURE_PROCESSOR_BUILD" "$GESTURE_PROCESSOR_TARGET"

  # copy common test (resources) keyboards
  cp -f "$KEYMAN_ROOT/common/test/keyboards/platform-rules/platformtest.js" "$KEYMAN_ROOT/$DEST/keyboards/"
  cp -f "$KEYMAN_ROOT/common/test/keyboards/test9469/build/test9469.js" "$KEYMAN_ROOT/$DEST/keyboards/"
  cp -f "$KEYMAN_ROOT/common/test/resources/keyboards/"*.js "$KEYMAN_ROOT/$DEST/keyboards/"
}

builder_run_action clean rm -rf "$KEYMAN_ROOT/$DEST" && rm -rf "$GESTURE_PROCESSOR_TARGET"
builder_run_action build do_copy