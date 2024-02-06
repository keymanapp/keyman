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

  BUILD_ROOT="${KEYMAN_ROOT}/web/build/app/browser"
  SRC_ROOT="${KEYMAN_ROOT}/web/src/app/browser/src"

  $BUNDLE_CMD    "${BUILD_ROOT}/obj/debug-main.js" \
    --out        "${BUILD_ROOT}/debug/keymanweb.es5.js" \
    --sourceRoot "@keymanapp/keyman/web/build/app/browser/debug"

  $BUNDLE_CMD    "${BUILD_ROOT}/obj/release-main.js" \
    --out        "${BUILD_ROOT}/release/keymanweb.es5.js" \
    --profile    "${BUILD_ROOT}/filesize-profile.es5.log" \
    --sourceRoot "@keymanapp/keyman/web/build/app/browser/release" \
    --minify

  $BUNDLE_CMD    "${SRC_ROOT}/debug-main.js" \
    --out        "${BUILD_ROOT}/debug/keymanweb.js" \
    --sourceRoot "@keymanapp/keyman/web/build/app/browser/debug" \
    --target     "es6"

  $BUNDLE_CMD    "${SRC_ROOT}/release-main.js" \
    --out        "${BUILD_ROOT}/release/keymanweb.js" \
    --profile    "${BUILD_ROOT}/filesize-profile.log" \
    --sourceRoot "@keymanapp/keyman/web/build/app/browser/release" \
    --minify \
    --target     "es6"

  $BUNDLE_CMD    "${BUILD_ROOT}/obj/test-index.js" \
    --out        "${BUILD_ROOT}/lib/index.mjs" \
    --sourceRoot "@keymanapp/keyman/web/build/app/browser/lib" \
    --format esm

  mkdir -p "$KEYMAN_ROOT/web/build/app/resources/osk"
  cp -R "$KEYMAN_ROOT/web/src/resources/osk/." "$KEYMAN_ROOT/web/build/app/resources/osk/"

  # Update the build/publish copy of our build artifacts
  prepare

  local PROFILE_DEST="$KEYMAN_ROOT/web/build/profiling/"
  mkdir -p "$PROFILE_DEST"
  cp "${BUILD_ROOT}/filesize-profile.log"                               "$PROFILE_DEST/web-engine-filesize.log"
  cp "${BUILD_ROOT}/filesize-profile.es5.log"                           "$PROFILE_DEST/web-engine-filesize.es5.log"
  cp "$KEYMAN_ROOT/common/web/lm-worker/build/filesize-profile.log"     "$PROFILE_DEST/lm-worker-filesize.log"
  cp "$KEYMAN_ROOT/common/web/lm-worker/build/filesize-profile.es5.log" "$PROFILE_DEST/lm-worker-filesize.es5.log"
}

builder_run_action configure verify_npm_setup
builder_run_action clean do_clean
builder_run_action build compile_and_copy
builder_run_action test test-headless-typescript $SUBPROJECT_NAME

# No headless tests for this child project.  Currently, DOM-based unit &
# integrated tests are run solely by the top-level $KEYMAN_ROOT/web project.