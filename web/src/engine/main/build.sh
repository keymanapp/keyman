#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

SUBPROJECT_NAME=engine/main
. "$KEYMAN_ROOT/web/common.inc.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# ################################ Main script ################################

builder_describe "Builds the Keyman Engine for Web's common top-level base classes." \
  "@/common/web/keyman-version" \
  "@/web/src/engine/keyboard" \
  "@/common/predictive-text" \
  "@/web/src/engine/interfaces build" \
  "@/web/src/engine/device-detect build" \
  "@/web/src/engine/js-processor build" \
  "@/web/src/engine/keyboard-storage build" \
  "@/web/src/engine/osk build" \
  "@/developer/src/kmc-model test" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "--ci+                     Set to utilize CI-based test configurations & reporting."

# Possible TODO?s
# "upload-symbols   Uploads build product to Sentry for error report symbolification.  Only defined for $DOC_BUILD_EMBED_WEB" \

builder_describe_outputs \
  configure   /node_modules \
  build       /web/build/$SUBPROJECT_NAME/lib/index.mjs

builder_parse "$@"

#### Build action definitions ####

do_build () {
  compile $SUBPROJECT_NAME

  $BUNDLE_CMD    "${KEYMAN_ROOT}/web/build/${SUBPROJECT_NAME}/obj/index.js" \
    --out        "${KEYMAN_ROOT}/web/build/${SUBPROJECT_NAME}/lib/index.mjs" \
    --format esm
}

builder_run_action configure verify_npm_setup
builder_run_action clean rm -rf "$KEYMAN_ROOT/web/build/$SUBPROJECT_NAME"
builder_run_action build do_build

# No headless tests for this child project.  Currently, DOM-based unit &
# integrated tests are run solely by the top-level $KEYMAN_ROOT/web project.