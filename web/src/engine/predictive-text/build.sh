#!/usr/bin/env bash
#
# Compile keymanweb predictive-text components.

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# ################################ Main script ################################

builder_describe "Builds predictive-text components used within Keyman Engine for Web (KMW)." \
  "clean" \
  "configure" \
  "build" \
  "test" \
  ":templates                Builds the model templates utlilized by compiled lexical models" \
  ":wordbreakers             Builds the wordbreakers provided for lexical model use" \
  ":worker-main              Builds the predictive-text worker interface module" \
  ":worker-thread            Builds the predictive-text worker" \
  ":_all                     (Meta build target used when targets are not specified)" \
  "--ci+                     Set to utilize CI-based test configurations & reporting."

# Possible TODO?
# "upload-symbols   Uploads build product to Sentry for error report symbolification.  Only defined for $DOC_BUILD_EMBED_WEB" \

builder_parse "$@"

config=release
if builder_is_debug_build; then
  config=debug
fi

builder_describe_outputs \
  configure                     "/node_modules" \
  build:templates               "/web/src/engine/predictive-text/build/obj/index.js" \
  build:wordbreakers            "/web/src/engine/wordbreakers/build/main/obj/index.js" \
  build:worker-main             "/web/src/engine/worker-main/build/obj/lmlayer.js" \
  build:worker-thread           "/web/src/engine/worker-thread/build/obj/worker-main.wrapped.js"

BUNDLE_CMD="node ${KEYMAN_ROOT}/web/src/tools/es-bundling/build/common-bundle.mjs"

#### Build action definitions ####

# We can run all clean & configure actions at once without much issue.

builder_run_child_actions clean

## Clean actions

builder_run_child_actions configure

## Build actions

builder_run_child_actions build:wordbreakers

builder_run_child_actions build:templates
builder_run_child_actions build:worker-thread
builder_run_child_actions build:worker-main

# If doing CI testing, the predictive-text child actions have their own build configuration.
# For local testing, though, we can allow them to proceed.
if ! builder_has_option --ci; then
  builder_run_child_actions test
fi