#!/usr/bin/env bash
#
# Compile keymanweb and copy compiled javascript and resources to output/embedded folder
#

# set -x
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

WORKING_DIRECTORY=`pwd`

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

# Imports common Web build-script definitions & functions
SUBPROJECT_NAME=engine/dom-utils
. ../../../.common.sh

# ################################ Main script ################################

builder_describe "Builds DOM-utility modules used by the Keyman Engine for Web (KMW)." \
  "@../../../../common/web/utils" \
  "@../../../../common/web/keyboard-processor" \
  "clean" \
  "configure" \
  "build"

# Possible TODO?
# "upload-symbols   Uploads build product to Sentry for error report symbolification.  Only defined for $DOC_BUILD_EMBED_WEB" \

builder_describe_outputs \
  configure    ../../../../node_modules \
  build        ../../../build/$SUBPROJECT_NAME/obj/index.js

builder_parse "$@"

#### Build action definitions ####

if builder_start_action configure; then
  verify_npm_setup

  builder_finish_action success configure
fi

if builder_start_action clean; then
  rm -rf ../../../build/$SUBPROJECT_NAME
  builder_finish_action success clean
fi

if builder_start_action build; then
  compile $SUBPROJECT_NAME

  builder_finish_action success build
fi