#!/usr/bin/env bash
#
# Compile keymanweb and copy compiled javascript and resources to output/embedded folder

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# ################################ Main script ################################

builder_set_child_base src
builder_describe "Builds engine modules for Keyman Engine for Web (KMW)." \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "coverage                  Create an HTML page with code coverage" \
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

builder_parse "$@"

config=release
if builder_is_debug_build; then
  config=debug
fi

builder_describe_outputs \
  configure                     "/node_modules" \
  build                         "/web/build/test/dom/cases/attachment/outputTargetForElement.spec.html" \
  build:app/browser             "/web/build/app/browser/lib/index.mjs" \
  build:app/webview             "/web/build/app/webview/${config}/keymanweb-webview.js" \
  build:app/ui                  "/web/build/app/ui/${config}/kmwuitoggle.js" \
  build:engine/attachment       "/web/build/engine/attachment/lib/index.mjs" \
  build:engine/device-detect    "/web/build/engine/device-detect/lib/index.mjs" \
  build:engine/dom-utils        "/web/build/engine/dom-utils/obj/index.js" \
  build:engine/events           "/web/build/engine/events/lib/index.mjs" \
  build:engine/element-wrappers "/web/build/engine/element-wrappers/lib/index.mjs" \
  build:engine/main             "/web/build/engine/main/lib/index.mjs" \
  build:engine/osk              "/web/build/engine/osk/lib/index.mjs" \
  build:engine/package-cache    "/web/build/engine/package-cache/lib/index.mjs" \
  build:engine/paths            "/web/build/engine/paths/lib/index.mjs" \
  build:samples                 "/web/src/samples/simplest/keymanweb.js" \
  build:tools                   "/web/build/tools/building/sourcemap-root/index.js" \
  build:test-pages              "/web/build/test-resources/sentry-manager.js"

BUNDLE_CMD="node ${KEYMAN_ROOT}/common/web/es-bundling/build/common-bundle.mjs"

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

precompile() {
  local DIR
  DIR="$1"

  builder_echo "Pre-compiling ${DIR}..."

  # pre-compile bundle for use in DOM testing. When running in the browser several
  # types built-in to Node aren't available, and @web/test-runner doesn't do
  # treeshaking when loading the imports. We work around by pre-compiling.
  for f in "${DIR}"/*.js; do
    ${BUNDLE_CMD}   "${f}" \
      --out         "${f%.js}".mjs \
      --format esm
  done
}

build_action() {
  builder_echo "Building auto tests..."
  tsc --project "${KEYMAN_ROOT}/web/src/test/auto/tsconfig.json"

  for dir in \
    "${KEYMAN_ROOT}/web/build/test/dom/cases"/*/ \
    "${KEYMAN_ROOT}/web/build/test/integrated/" \
    "${KEYMAN_ROOT}/web/build/test/integrated/cases/";
  do
    precompile "${dir}"
  done

  cp "${KEYMAN_ROOT}/web/src/test/auto/dom/cases/attachment/outputTargetForElement.spec.html" \
    "${KEYMAN_ROOT}/web/build/test/dom/cases/attachment/"
}

test_action() {
  TEST_OPTS=
  if builder_has_option --ci; then
    TEST_OPTS=--ci
  fi
  ./test.sh "${TEST_OPTS}"
}

coverage_action() {
  builder_echo "Creating coverage report..."
  cd "$KEYMAN_ROOT"
  mkdir -p web/build/coverage/tmp
  find . -type f -name coverage-\*.json -print0 | xargs -0 cp -t web/build/coverage/tmp
  c8 report --config web/.c8rc.json ---reporter html --clean=false --reports-dir=web/build/coverage
  rm -rf web/build/coverage/tmp
  cd web
}

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

# Build tests
builder_run_action build build_action

# Run tests
builder_run_child_actions test
builder_run_action test test_action

# Create coverage report
builder_run_action coverage coverage_action
