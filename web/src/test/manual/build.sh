#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

# ################################ Main script ################################

builder_describe "Builds the Keyman Engine for Web's sample page setups." \
  "@/common/web/sentry-manager build" \
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
}

builder_run_action clean rm -rf "$KEYMAN_ROOT/$DEST"
builder_run_action build do_copy