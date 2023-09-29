#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

SUBPROJECT_NAME=app/browser
. "$KEYMAN_ROOT/web/common.inc.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

# ################################ Main script ################################

builder_describe "Builds the Keyman Engine for Web's website-integrating version for use in non-puppeted browsers." \
  "@/web/src/engine/attachment build" \
  "@/web/src/engine/main build" \
  "@/web/src/tools/building/sourcemap-root" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "--ci+                     Set to utilize CI-based test configurations & reporting."

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

do_clean() {
  rm -rf "$KEYMAN_ROOT/web/build/$SUBPROJECT_NAME"
  rm -rf "$KEYMAN_ROOT/web/build/publish"
}

compile_and_copy() {
  local COMPILE_FLAGS=
  if builder_has_option --ci; then
    COMPILE_FLAGS=--ci
  fi
  compile $SUBPROJECT_NAME $COMPILE_FLAGS

  mkdir -p "$KEYMAN_ROOT/web/build/app/resources/osk"
  cp -R "$KEYMAN_ROOT/web/src/resources/osk/." "$KEYMAN_ROOT/web/build/app/resources/osk/"

  # Update the build/publish copy of our build artifacts
  prepare

  local PROFILE_DEST="$KEYMAN_ROOT/web/build/profiling/"
  mkdir -p "$PROFILE_DEST"
  cp "$KEYMAN_ROOT/web/build/app/browser/filesize-profile.log"      "$PROFILE_DEST/web-engine-filesize.log"
  cp "$KEYMAN_ROOT/common/web/lm-worker/build/filesize-profile.log" "$PROFILE_DEST/lm-worker-filesize.log"
}

builder_run_action configure verify_npm_setup
builder_run_action clean do_clean
builder_run_action build compile_and_copy

# No headless tests for this child project.  Currently, DOM-based unit &
# integrated tests are run solely by the top-level $KEYMAN_ROOT/web project.