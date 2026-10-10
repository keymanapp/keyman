#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# ################################ Main script ################################

builder_describe "Builds the Keyman Engine for Web's sample page setups." \
  "@/common/web/sentry-manager build" \
  "@/web/src/engine/predictive-text build" \
  "@/web/src/app/browser build" \
  "@/web/src/app/ui build" \
  "@/web/src/tools build" \
  "configure           Does nothing for this project" \
  "clean" \
  "build" \
  "test                Does nothing for this project"

builder_parse "$@"

DEST="web/build/test-resources"

builder_describe_outputs \
  build       "/${DEST}/sentry-manager.js"

#### Resource paths ####

BUNDLE_CMD="node $KEYMAN_ROOT/common/tools/es-bundling/build/common-bundle.mjs"

#### Build action definitions ####

function do_copy() {
  mkdir -p "${KEYMAN_ROOT}/${DEST}/keyboards"

  # The next two lines are needed for the sentry-integration manual test page.
  cp "${KEYMAN_ROOT}/common/web/sentry-manager/build/lib/index.js"      "${KEYMAN_ROOT}/${DEST}/sentry-manager.js"
  cp "${KEYMAN_ROOT}/common/web/sentry-manager/build/lib/index.js.map"  "${KEYMAN_ROOT}/${DEST}/sentry-manager.js.map"

  cp "${KEYMAN_ROOT}/common/web/types/tests/fixtures/kmx/khmer_angkor.kmx" "${KEYMAN_ROOT}/${DEST}/keyboards/"

  # copy common test (resources) keyboards
  cp -f "${KEYMAN_ROOT}/common/test/keyboards/platform-rules/platformtest.js" "${KEYMAN_ROOT}/${DEST}/keyboards/"
  cp -f "${KEYMAN_ROOT}/common/test/keyboards/test9469/build/test9469.js" "${KEYMAN_ROOT}/${DEST}/keyboards/"
  cp -f "${KEYMAN_ROOT}/common/test/resources/keyboards/"*.js "${KEYMAN_ROOT}/${DEST}/keyboards/"

  WORDBREAK_DEMO_TARGET="$KEYMAN_ROOT/web/build/demos/wordbreaker-libs/"

  mkdir -p "$WORDBREAK_DEMO_TARGET"

  $BUNDLE_CMD    "${KEYMAN_ROOT}/web/src/engine/predictive-text/templates/build/obj/index.js" \
    --out        "${WORDBREAK_DEMO_TARGET}/templates.js" \
    --format esm

  # One of the functions (timedPromise) is quite helpful for automated testing, even in the DOM.
  # So, to make sure it's easily-accessible for the DOM-based tests...
  $BUNDLE_CMD    "${KEYMAN_ROOT}/web/src/engine/predictive-text/wordbreakers/build/main/obj/index.js" \
    --out        "${WORDBREAK_DEMO_TARGET}/wordbreakers.js" \
    --format esm
}

builder_run_action clean   rm -rf "${KEYMAN_ROOT}/${DEST}"
builder_run_action build   do_copy
