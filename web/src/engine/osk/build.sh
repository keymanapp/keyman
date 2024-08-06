#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

SUBPROJECT_NAME=engine/osk
. "$KEYMAN_ROOT/web/common.inc.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# ################################ Main script ################################

builder_describe "Builds the Keyman Engine for Web's On-Screen Keyboard package (OSK)." \
  "@/common/web/input-processor build" \
  "@/common/web/gesture-recognizer build" \
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
  configure   /web/src/resources/osk/keymanweb-osk.ttf \
  build       /web/build/$SUBPROJECT_NAME/lib/index.mjs

builder_parse "$@"

#### Build action definitions ####

do_configure() {
  verify_npm_setup
  cp "$KEYMAN_ROOT/common/resources/fonts/keymanweb-osk.ttf" "$KEYMAN_ROOT/web/src/resources/osk/"
}

do_build() {
  compile "${SUBPROJECT_NAME}"

  $BUNDLE_CMD    "${KEYMAN_ROOT}/web/build/${SUBPROJECT_NAME}/obj/index.js" \
    --out        "${KEYMAN_ROOT}/web/build/${SUBPROJECT_NAME}/lib/index.mjs" \
    --format esm

  echo "Validating gesture model and set references"
  node validate-gesture-specs.js
}

builder_run_action configure do_configure
builder_run_action clean rm -rf "${KEYMAN_ROOT}/web/build/${SUBPROJECT_NAME}"
builder_run_action build do_build
builder_run_action test test-headless-typescript "${SUBPROJECT_NAME}"