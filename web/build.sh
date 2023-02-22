#!/usr/bin/env bash
#
# Compile keymanweb and copy compiled javascript and resources to output/embedded folder
#

# set -x
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

WORKING_DIRECTORY=`pwd`

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

# ################################ Main script ################################

# Definition of global compile constants

MAIN=engine/main     # Covers all engine code, including submodules like those listed below.
KEYBOARDCACHE=engine/keyboard-cache
OSK=engine/osk

BUILD_BASE=build

OUTPUT_DIR=obj

SOURCE="src"

# Ensures that we rely first upon the local npm-based install of Typescript.
# (Facilitates automated setup for build agents.)
PATH="../../../node_modules/.bin:$PATH"

compiler="npm run tsc --"
compilecmd="$compiler"

builder_set_child_base src
builder_describe "Builds engine modules for Keyman Engine for Web (KMW)." \
  "clean" \
  "configure" \
  "build" \
  ":engine/configuration     Subset used to configure KMW" \
  ":engine/device-detect     Subset used for device-detection " \
  ":engine/dom-utils         A common subset of function used for DOM calculations, layout, etc" \
  ":engine/element-wrappers  Subset used to integrate with website elements" \
  ":engine/keyboard-cache    Subset used to collate keyboards and request them from the cloud" \
  ":engine/main              Builds all common code used by KMW's app/-level targets" \
  ":engine/osk               Builds the Web OSK module"

# Possible TODO?
# "upload-symbols   Uploads build product to Sentry for error report symbolification.  Only defined for $DOC_BUILD_EMBED_WEB" \

builder_describe_outputs \
  configure                  ../node_modules

builder_parse "$@"

#### Build action definitions ####

##################### TODO:  call child action, verify things work as expected!

# We can run all clean & configure actions at once without much issue.

builder_run_child_actions clean configure

## Clean actions

# If a full-on general clean was requested, we can nuke the entire build folder.
if builder_start_action clean; then
  rm -rf ./build
  builder_finish_action success clean
fi

## Build actions

builder_run_child_actions build:engine/device-detect
builder_run_child_actions build:engine/dom-utils
builder_run_child_actions build:engine/element-wrappers
builder_run_child_actions build:engine/keyboard-cache

# Uses engine/device-detect
builder_run_child_actions build:engine/configuration

# Uses engine/device-detect + engine/dom-utils
builder_run_child_actions build:engine/osk

# if builder_start_action build:main; then
#   compile $MAIN

#   builder_finish_action success build:main
# fi