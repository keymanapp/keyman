#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

SUBPROJECT_NAME=engine/element-wrappers
. "$KEYMAN_ROOT/web/common.inc.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

# ################################ Main script ################################

builder_describe "Builds DOM-based OutputTarget subclasses used by the Keyman Engine for Web (KMW)." \
  "@/common/web/keyboard-processor" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "--ci+                     Set to utilize CI-based test configurations & reporting."

# Possible TODO?
# "upload-symbols   Uploads build product to Sentry for error report symbolification.  Only defined for $DOC_BUILD_EMBED_WEB" \

builder_describe_outputs \
  configure   /node_modules \
  build       /web/build/$SUBPROJECT_NAME/lib/index.mjs

builder_parse "$@"

#### Build action definitions ####

builder_run_action configure verify_npm_setup
builder_run_action clean rm -rf "$KEYMAN_ROOT/web/build/$SUBPROJECT_NAME"
builder_run_action build compile $SUBPROJECT_NAME

# No headless tests for this child project.  Currently, DOM-based unit &
# integrated tests are run solely by the top-level $KEYMAN_ROOT/web project.