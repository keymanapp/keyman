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
  "@/common/models/templates build" \
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

BUNDLE_CMD="node $KEYMAN_ROOT/common/web/es-bundling/build/common-bundle.mjs"

TEMPLATES_SRC="$KEYMAN_ROOT/common/models/templates/build/lib/index.mjs"
TEMPLATES_MAP="$KEYMAN_ROOT/common/models/templates/build/lib/index.mjs"
WORDBREAKING_SRC="$KEYMAN_ROOT/common/models/wordbreakers/build/lib/index.mjs"
WORDBREAKING_MAP="$KEYMAN_ROOT/common/models/wordbreakers/build/lib/index.mjs"

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

  WORDBREAK_DEMO_TARGET="$KEYMAN_ROOT/web/build/demos/wordbreaker-libs/"

  mkdir -p "$WORDBREAK_DEMO_TARGET"

  # pushd "$KEYMAN_ROOT/common/models/templates" > /dev/null

  $BUNDLE_CMD    "${KEYMAN_ROOT}/common/models/templates/build/obj/index.js" \
    --out        "${WORDBREAK_DEMO_TARGET}/templates.mjs" \
    --format esm

  # popd
  # pushd "$KEYMAN_ROOT/common/models/wordbreakers" > /dev/null

  # One of the functions (timedPromise) is quite helpful for automated testing, even in the DOM.
  # So, to make sure it's easily-accessible for the DOM-based tests...
  $BUNDLE_CMD    "${KEYMAN_ROOT}/common/models/wordbreakers/build/obj/index.js" \
    --out        "${WORDBREAK_DEMO_TARGET}/wordbreakers.mjs" \
    --format esm

  # popd

  # cp "$TEMPLATES_SRC" "$WORDBREAK_DEMO_TARGET"
  # cp "$TEMPLATES_MAP" "$WORDBREAK_DEMO_TARGET"
  # cp "$WORDBREAKING_SRC" "$WORDBREAK_DEMO_TARGET"
  # cp "$WORDBREAKING_MAP" "$WORDBREAK_DEMO_TARGET"
}

builder_run_action clean rm -rf "$KEYMAN_ROOT/$DEST"
builder_run_action build do_copy