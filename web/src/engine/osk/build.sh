#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

SUBPROJECT_NAME=engine/osk
. "$KEYMAN_ROOT/web/common.inc.sh"
. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"

# ################################ Main script ################################

builder_describe "Builds the Keyman Engine for Web's On-Screen Keyboard package (OSK)." \
  "@/web/src/engine/keyboard build" \
  "@/web/src/engine/interfaces build" \
  "@/web/src/engine/dom-utils build" \
  "@/web/src/engine/events build" \
  "@/web/src/engine/osk/gesture-processor" \
  "clean" \
  "configure" \
  "build" \
  "test"

# Possible TODO?s
# "upload-symbols   Uploads build product to Sentry for error report symbolification.  Only defined for $DOC_BUILD_EMBED_WEB" \

builder_describe_outputs \
  configure   /web/src/resources/osk/keymanweb-osk.ttf \
  build       /web/build/$SUBPROJECT_NAME/lib/index.mjs

builder_parse "$@"

#### Build action definitions ####

do_clean() {
  rm -rf "${KEYMAN_ROOT}/web/build/${SUBPROJECT_NAME}"

  ./gesture-processor/build.sh clean
}

do_configure() {
  node_select_version_and_npm_ci
  cp "$KEYMAN_ROOT/common/resources/fonts/keymanweb-osk.ttf" "$KEYMAN_ROOT/web/src/resources/osk/"
}

do_build() {
  compile "${SUBPROJECT_NAME}"

  node "${LIB_BUNDLER}"    "${KEYMAN_ROOT}/web/build/${SUBPROJECT_NAME}/obj/index.js" \
    --out                  "${KEYMAN_ROOT}/web/build/${SUBPROJECT_NAME}/lib/index.mjs" \
    --format esm

  echo "Validating gesture model and set references"
  node validate-gesture-specs.js
}

do_test() {
  test-headless-typescript "${SUBPROJECT_NAME}"

  ./gesture-processor/build.sh test
}

builder_run_action configure do_configure
builder_run_action clean do_clean
builder_run_action build do_build
builder_run_action test do_test
