#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

SUBPROJECT_NAME=app/ui
. "$KEYMAN_ROOT/web/common.inc.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# ################################ Main script ################################

builder_describe "Builds the Keyman Engine for Web's desktop form-factor keyboard selection modules." \
  "@/web/src/app/browser build" \
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
  build       /web/build/$SUBPROJECT_NAME/$config/kmwuitoggle.js

#### Build action definitions ####

do_clean() {
  rm -rf "$KEYMAN_ROOT/web/build/$SUBPROJECT_NAME"
  rm -rf "$KEYMAN_ROOT/web/build/publish"
}

compile_and_copy() {
  compile $SUBPROJECT_NAME

  BUILD_ROOT="${KEYMAN_ROOT}/web/build/app/ui"

  types=(button float toggle toolbar)
  for type in ${types[@]}
  do
    filename="kmwui${type}"
    $BUNDLE_CMD    "${BUILD_ROOT}/obj/$filename.js" \
      --out        "${BUILD_ROOT}/debug/$filename.js" \
      --sourceRoot "@keymanapp/keyman/web/build/app/ui/debug"

    $BUNDLE_CMD    "${BUILD_ROOT}/obj/$filename.js" \
      --out        "${BUILD_ROOT}/release/$filename.js" \
      --sourceRoot "@keymanapp/keyman/web/build/app/ui/release" \
      --minify
  done

  mkdir -p "$KEYMAN_ROOT/web/build/app/resources/ui"
  cp -R "$KEYMAN_ROOT/web/src/resources/ui/." "$KEYMAN_ROOT/web/build/app/resources/ui/"

  # Copy Keyman Core build artifacts for local reference
  cp "${KEYMAN_ROOT}/web/build/engine/core-processor/obj/import/core/"km-core.{js,wasm} "${KEYMAN_ROOT}/web/build/app/ui/debug/"
  cp "${KEYMAN_ROOT}/web/build/engine/core-processor/obj/import/core/"km-core.{js,wasm} "${KEYMAN_ROOT}/web/build/app/ui/release/"

  # Update the build/publish copy of our build artifacts
  prepare
}

builder_run_action configure verify_npm_setup
builder_run_action clean do_clean
builder_run_action build compile_and_copy

# No headless tests for this child project.  Currently, DOM-based unit &
# integrated tests are run solely by the top-level $KEYMAN_ROOT/web project.