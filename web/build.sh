#!/usr/bin/env bash
#
# Compile keymanweb and copy compiled javascript and resources to output/embedded folder
#

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

# ################################ Main script ################################

# Ensures that we rely first upon the local npm-based install of Typescript.
# (Facilitates automated setup for build agents.)
# TODO: this should be removeable given set_keyman_standard_build_path does this in build-utils.sh (and relative paths are dodgy in $PATH!)
PATH="../node_modules/.bin:$PATH"

builder_set_child_base src
builder_describe "Builds engine modules for Keyman Engine for Web (KMW)." \
  "clean" \
  "configure" \
  "build" \
  "test" \
  ":app/webview              A puppetable version of KMW designed for use in a host app's WebView" \
  ":engine/configuration     Subset used to configure KMW" \
  ":engine/device-detect     Subset used for device-detection " \
  ":engine/dom-utils         A common subset of function used for DOM calculations, layout, etc" \
  ":engine/element-wrappers  Subset used to integrate with website elements" \
  ":engine/keyboard-cache    Subset used to collate keyboards and request them from the cloud" \
  ":engine/main              Builds all common code used by KMW's app/-level targets" \
  ":engine/osk               Builds the Web OSK module"

# ":app/browser              The website-integrating, browser-based version of KMW" \

# Possible TODO?
# "upload-symbols   Uploads build product to Sentry for error report symbolification.  Only defined for $DOC_BUILD_EMBED_WEB" \

builder_describe_outputs \
  configure                      /node_modules

# build:app/webview              build/app/webview/lib/index.js \

  # TODO:  app/ui linkage.

builder_parse "$@"

#### Build action definitions ####

##################### TODO:  call child action, verify things work as expected!

# We can run all clean & configure actions at once without much issue.

builder_run_child_actions clean
builder_run_child_actions configure

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

# Uses engine/dom-utils
builder_run_child_actions build:engine/osk

# Uses engine/osk (due to resource-path config interface)
builder_run_child_actions build:engine/configuration

# Uses engine/config (also due to resource-path config interface, but for the
# more complete version of that interface)
builder_run_child_actions build:engine/keyboard-cache

# Uses engine/configuration, engine/device-detect, engine/keyboard-cache, & engine/osk
builder_run_child_actions build:engine/main

# Uses all but engine/element-wrappers
builder_run_child_actions build:app/webview

# Uses literally everything `engine/` above
# Is not yet compilable due to unmodularized components.
# builder_run_child_actions build:app/browser

builder_run_child_actions test

if builder_start_action test; then
  ./test.sh :engine

  builder_finish_action success test
fi

builder_die "Modularization work is not yet complete; builds dependent on this will fail."
