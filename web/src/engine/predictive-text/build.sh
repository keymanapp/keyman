#!/usr/bin/env bash
#
# Compile keymanweb predictive-text components.

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"

# ################################ Main script ################################

builder_describe "Builds predictive-text components used within Keyman Engine for Web (KMW)." \
  "clean" \
  "configure" \
  "build" \
  "test" \
  ":templates                Builds the model templates utilized by compiled lexical models" \
  ":wordbreakers             Builds the wordbreakers provided for lexical model use" \
  ":worker-main              Builds the predictive-text worker interface module" \
  ":worker-thread            Builds the predictive-text worker"

# Possible TODO?
# "upload-symbols   Uploads build product to Sentry for error report symbolification.  Only defined for $DOC_BUILD_EMBED_WEB" \

builder_parse "$@"

builder_describe_outputs \
  configure                     "/node_modules" \
  build:templates               "/web/src/engine/predictive-text/templates/build/obj/index.js" \
  build:wordbreakers            "/web/src/engine/predictive-text/wordbreakers/build/main/obj/index.js" \
  build:worker-main             "/web/src/engine/predictive-text/worker-main/build/lib/index.mjs" \
  build:worker-thread           "/web/src/engine/predictive-text/worker-thread/build/lib/worker-main.wrapped.js"

#### Build action definitions ####

# We can run all clean & configure actions at once without much issue.

builder_run_child_actions clean
builder_run_child_actions configure

## Build actions

builder_run_child_actions build:wordbreakers
builder_run_child_actions build:templates
builder_run_child_actions build:worker-thread
builder_run_child_actions build:worker-main

builder_run_child_actions test
