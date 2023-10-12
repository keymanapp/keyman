#!/usr/bin/env bash
#
# Compile keymanweb and copy compiled javascript and resources to output/embedded folder

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

# ################################ Main script ################################

builder_set_child_base src
builder_describe "Builds engine modules for Keyman Engine for Web (KMW)." \
  "clean" \
  "configure" \
  "build" \
  "test" \
  ":app/browser              The form of Keyman Engine for Web for use on websites" \
  ":app/webview              A puppetable version of KMW designed for use in a host app's WebView" \
  ":app/ui                   Builds KMW's desktop form-factor keyboard-selection UI modules" \
  ":engine/attachment        Subset used for detecting valid page contexts for use in text editing " \
  ":engine/device-detect     Subset used for device-detection " \
  ":engine/dom-utils         A common subset of function used for DOM calculations, layout, etc" \
  ":engine/events            Specialized classes utilized to support KMW API events" \
  ":engine/element-wrappers  Subset used to integrate with website elements" \
  ":engine/main              Builds all common code used by KMW's app/-level targets" \
  ":engine/osk               Builds the Web OSK module" \
  ":engine/package-cache     Subset used to collate keyboards and request them from the cloud" \
  ":engine/paths             Subset used to configure KMW" \
  ":samples                  Builds all needed resources for the KMW sample-page set" \
  ":tools                    Builds engine-related development resources" \
  ":test-pages=src/test/manual   Builds resources needed for the KMW manual testing pages" \
  "--ci+                     Set to utilize CI-based test configurations & reporting."

# Possible TODO?
# "upload-symbols   Uploads build product to Sentry for error report symbolification.  Only defined for $DOC_BUILD_EMBED_WEB" \

builder_describe_outputs \
  configure                      /node_modules

builder_parse "$@"

#### Build action definitions ####

##################### TODO:  call child action, verify things work as expected!

# We can run all clean & configure actions at once without much issue.

builder_run_child_actions clean

## Clean actions

###--- Future tie-in:  if #8831 gets accepted, uncomment the next two lines. ---###
# # If a full-on general clean was requested, we can nuke the entire build folder.
# builder_run_action clean:project rm -rf ./build

builder_run_child_actions configure

## Build actions

builder_run_child_actions build:engine/device-detect
builder_run_child_actions build:engine/dom-utils
builder_run_child_actions build:engine/element-wrappers
builder_run_child_actions build:engine/events

# Uses engine/dom-utils
builder_run_child_actions build:engine/osk

# Uses engine/element-wrappers
builder_run_child_actions build:engine/attachment

# Uses engine/osk (due to resource-path config interface)
builder_run_child_actions build:engine/paths

# Uses engine/config (also due to resource-path config interface, but for the
# more complete version of that interface)
builder_run_child_actions build:engine/package-cache

# Uses engine/paths, engine/device-detect, engine/package-cache, & engine/osk
builder_run_child_actions build:engine/main

# Uses all but engine/element-wrappers and engine/attachment
builder_run_child_actions build:app/webview

# Uses literally everything `engine/` above
builder_run_child_actions build:app/browser

# Uses app/browser above for engine type-defs
builder_run_child_actions build:app/ui

# Needs both app/browser and app/ui.
builder_run_child_actions build:samples

builder_run_child_actions build:tools

# Some test pages refer to KMW tools.
builder_run_child_actions build:test-pages

builder_run_child_actions test

if builder_start_action test; then
  TEST_OPTS=
  if builder_has_option --ci; then
    TEST_OPTS=--ci
  fi
  ./test.sh $TEST_OPTS

  builder_finish_action success test
fi
