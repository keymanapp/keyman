#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# ################################ Main script ################################

builder_describe "Builds the Keyman Engine for Web's sample page setups." \
  "@/common/web/sentry-manager build" \
  "@/web/src/app/browser build" \
  "@/web/src/app/ui build" \
  "@/web/src/tools build" \
  "configure           Does nothing for this project" \
  "clean" \
  "build" \
  "test                Does nothing for this project" \
  "--ci                Does nothing for this project"

# Possible TODO?s
# "upload-symbols   Uploads build product to Sentry for error report symbolification.  Only defined for $DOC_BUILD_EMBED_WEB" \

builder_parse "$@"

DEST="web/build/test-resources"

builder_describe_outputs \
  build       "/$DEST/sentry-manager.js"

#### Resource paths ####

SENTRY_MANAGER_SRC="$KEYMAN_ROOT/common/web/sentry-manager/build/lib/index.js"
SENTRY_MANAGER_MAP="$KEYMAN_ROOT/common/web/sentry-manager/build/lib/index.js.map"
SENTRY_SRC="$KEYMAN_ROOT/node_modules/@sentry/browser/build/bundle.min.js"
SENTRY_MAP="$KEYMAN_ROOT/node_modules/@sentry/browser/build/bundle.min.js.map"

#### Build action definitions ####

function do_copy() {
  mkdir -p "$KEYMAN_ROOT/$DEST"

  # The next four lines are needed for the sentry-integration manual test page.
  cp "$SENTRY_MANAGER_SRC"  "$KEYMAN_ROOT/$DEST/sentry-manager.js"
  cp "$SENTRY_MANAGER_MAP"  "$KEYMAN_ROOT/$DEST/sentry-manager.js.map"
  cp "$SENTRY_SRC"          "$KEYMAN_ROOT/$DEST/sentry-bundle.min.js"
  cp "$SENTRY_MAP"          "$KEYMAN_ROOT/$DEST/sentry-bundle.min.js.map"

  GESTURE_RECOGNIZER_BUILD="$KEYMAN_ROOT/common/web/gesture-recognizer/build/lib/."
  GESTURE_RECOGNIZER_TARGET="$KEYMAN_ROOT/web/build/engine/gesture-recognizer/lib/"

  mkdir -p "$GESTURE_RECOGNIZER_TARGET"
  cp -a "$GESTURE_RECOGNIZER_BUILD" "$GESTURE_RECOGNIZER_TARGET"
}

builder_run_action clean rm -rf "$KEYMAN_ROOT/$DEST"
builder_run_action build do_copy
