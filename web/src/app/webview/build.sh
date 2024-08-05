#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

SUBPROJECT_NAME=app/webview
. "$KEYMAN_ROOT/web/common.inc.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# ################################ Main script ################################

builder_describe "Builds the Keyman Engine for Web's puppetable version designed for use within WebViews." \
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
  build       /web/build/$SUBPROJECT_NAME/$config/keymanweb-webview.js

#### Build action definitions ####

compile_and_copy() {
  compile $SUBPROJECT_NAME

  BUILD_ROOT="${KEYMAN_ROOT}/web/build/app/webview"
  SRC_ROOT="${KEYMAN_ROOT}/web/src/app/webview/src"

  $BUNDLE_CMD    "${SRC_ROOT}/debug-main.js" \
    --out        "${BUILD_ROOT}/debug/keymanweb-webview.js" \
    --sourceRoot "@keymanapp/keyman/web/build/app/webview/debug" \
    --target     "es6"

  $BUNDLE_CMD    "${SRC_ROOT}/release-main.js" \
    --out        "${BUILD_ROOT}/release/keymanweb-webview.js" \
    --profile    "${BUILD_ROOT}/filesize-profile.log" \
    --sourceRoot "@keymanapp/keyman/web/build/app/webview/release" \
    --minify \
    --target     "es6"

  mkdir -p "$KEYMAN_ROOT/web/build/app/resources/osk"
  cp -R "$KEYMAN_ROOT/web/src/resources/osk/." "$KEYMAN_ROOT/web/build/app/resources/osk/"

  # Clean the sourcemaps of .. and . components
  for script in "$KEYMAN_ROOT/web/build/$SUBPROJECT_NAME/debug/"*.js; do
    sourcemap="$script.map"
    node "$KEYMAN_ROOT/web/build/tools/building/sourcemap-root/index.js" \
      "$script" "$sourcemap" --clean --inline
  done

  # Do NOT inline sourcemaps for release builds - we don't want them to affect
  # load time.
  for script in "$KEYMAN_ROOT/web/build/$SUBPROJECT_NAME/release/"*.js; do
    sourcemap="$script.map"
    node "$KEYMAN_ROOT/web/build/tools/building/sourcemap-root/index.js" \
      "$script" "$sourcemap" --clean
  done

  node map-polyfill-bundler.js

  # For dependent test pages.
  "$KEYMAN_ROOT/web/src/test/manual/embed/android-harness/build.sh"
}

builder_run_action configure verify_npm_setup
builder_run_action clean rm -rf "$KEYMAN_ROOT/web/build/$SUBPROJECT_NAME"
builder_run_action build compile_and_copy