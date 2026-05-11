#!/usr/bin/env bash
#
# Compile keymanweb and copy compiled javascript and resources to output/embedded folder

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"

# ################################ Main script ################################

builder_set_child_base src
builder_describe "Builds engine modules for Keyman Engine for Web (KMW)." \
  \
  "@/common/tools/es-bundling        build" \
  "@/resources/tools/check-markdown  test:help" \
  \
  "clean" \
  "configure" \
  "build" \
  "start                     Starts the test server" \
  "test" \
  "coverage                  Create an HTML page with code coverage report" \
  \
  ":app/browser              KeymanWeb build for use on websites" \
  ":app/webview              KeymanWeb build for embedding in Keyman for Android and Keyman for iOS" \
  ":app/ui                   KeymanWeb desktop form-factor keyboard-selection UI modules" \
  ":common/web-utils         Shared utils" \
  ":engine                   Keyman Engine for Web" \
  ":help                     Online documentation" \
  ":samples                  Resources for the KMW sample-page set" \
  ":tools/building           Build tools" \
  ":tools/testing            Test tools" \
  ":test-pages=src/test/manual   Builds resources needed for the KMW manual testing pages" \
  ":_all                     (Meta build target used when targets are not specified)" \
  \
  "--test-dom-only           For test, run only DOM-oriented unit tests (reduced footprint, nothing browser-specific)" \
  "--test-integrated-only    For test, run only KMW's integration test suite" \
  "--test-e2e-only           For test, run only KMW's end-to-end test suite" \
  "--test-inspect            For test, run browser-based unit tests in an inspectable mode"

builder_parse "$@"

config=release
if builder_is_debug_build; then
  config=debug
fi

builder_describe_outputs \
  configure                     "/node_modules" \
  build                         "/web/build/test/dom/cases/attachment/textStoreForElement.tests.html" \
  build:app/browser             "/web/build/app/browser/lib/index.mjs" \
  build:app/webview             "/web/build/app/webview/${config}/keymanweb-webview.js" \
  build:app/ui                  "/web/build/app/ui/${config}/kmwuitoggle.js" \
  build:common/web-utils        "/web/build/common/web-utils/lib/index.mjs" \
  build:engine                  "/web/build/engine/lib/index.mjs" \
  build:samples                 "/web/src/samples/simplest/keymanweb.js" \
  build:tools/building          "/web/build/tools/building/sourcemap-root/index.js" \
  build:tools/testing           "/web/build/tools/testing/test-utils/lib/index.d.ts" \
  build:test-pages              "/web/build/test-resources/sentry-manager.js"

#### Build action definitions ####

# We can run all clean & configure actions at once without much issue.

## Clean actions

builder_run_action clean:_all rm -rf build/

builder_run_child_actions clean

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
    node_es_bundle  "${f}" \
      --out         "${f%.js}".mjs \
      --format esm
  done
}

build_tests_action() {
  builder_echo "Building auto tests..."

  # The currently-bundled declaration file for gesture-processor generates
  # errors when compiling against it with current tsc versions.
  rm -f "${KEYMAN_ROOT}/node_modules/promise-status-async/lib/index.d.ts"

  tsc -b "${KEYMAN_ROOT}/web/src/test/auto/tsconfig.json"

  builder_echo "Copying some files"

  for dir in \
    "${KEYMAN_ROOT}/web/build/test/dom/cases"/*/ \
    "${KEYMAN_ROOT}/web/build/test/integrated/" \
    "${KEYMAN_ROOT}/web/build/test/integrated/cases/";
  do
    precompile "${dir}"
  done

  cp "${KEYMAN_ROOT}/web/src/test/auto/dom/cases/attachment/textStoreForElement.tests.html" \
    "${KEYMAN_ROOT}/web/build/test/dom/cases/attachment/"
}

coverage_action() {
  builder_echo "Creating coverage report..."

  # Always ensure our collation folder is clean, just in case something went wrong on a prior run.
  rm -rf build/coverage/tmp

  # Also clean out the sub-report directories + main index.html; don't keep old ones if we fail!
  rm -rf build/coverage/app
  rm -rf build/coverage/engine
  rm -f  build/coverage/index.html

  # Collate the results into a single folder
  mkdir build/coverage/tmp
  find . -type f -name coverage-\*.json -print0 | xargs -0 cp -t build/coverage/tmp

  # Note for maintainers:  the .c8rc.json config needs to be written from the perspective of
  # the working directory - including its `include` and `exclude` entries.  All ignores and
  # such must be redefined in whatever that top-level .c8rc.json is.
  c8 report --reporter html

  # No need to keep the collation folder around afterward; we can always redo that work.
  rm -rf build/coverage/tmp
}

builder_run_child_actions build:tools/building

builder_run_child_actions build:common/web-utils

builder_run_child_actions build:engine
# builder_run_child_actions build:engine/predictive-text

# Uses all but engine/element-text-stores and engine/attachment
builder_run_child_actions build:app/webview

# Uses literally everything `engine/` above
builder_run_child_actions build:app/browser

# Uses app/browser above for engine type-defs
builder_run_child_actions build:app/ui

# Needs both app/browser and app/ui.
builder_run_child_actions build:samples

# Some test pages refer to KMW tools.
builder_run_child_actions build:test-pages

# Build tests
builder_run_child_actions build:tools/testing
builder_run_action build:_all build_tests_action

# Run tests
builder_run_child_actions test

function is_test_included() {
  local test_opt=$1
  local run_dom_tests=true run_integrated_tests=true run_e2e_tests=true
  local var_name="run_${test_opt}_tests"

  if builder_has_option --test-integrated-only || builder_has_option --test-e2e-only; then
    run_dom_tests=false
  fi
  if builder_has_option --test-dom-only || builder_has_option --test-e2e-only; then
    run_integrated_tests=false
  fi
  if builder_has_option --test-dom-only || builder_has_option --test-integrated-only; then
    run_e2e_tests=false
  fi

  ${!var_name}
}

function do_browser_tests() {
  # Browser-based tests: common configs & kill-switches

  # Select the right CONFIG file.
  local WTR_CONFIG=
  if builder_is_ci_build; then
    WTR_CONFIG=.CI
    export KEYMAN_IS_CI_BUILD=1
  fi

  # Prepare the flags for the karma command.
  local WTR_INSPECT=
  if builder_has_option --test-inspect; then
    WTR_INSPECT="--manual"
  fi

  pushd "${KEYMAN_ROOT}"
  if is_test_included dom; then
    web-test-runner --config "web/src/test/auto/dom/web-test-runner${WTR_CONFIG}.config.mjs" ${WTR_INSPECT}
  fi
  if is_test_included integrated; then
    web-test-runner --config "web/src/test/auto/integrated/web-test-runner${WTR_CONFIG}.config.mjs" ${WTR_INSPECT}
  fi
  if is_test_included e2e; then
    npx playwright test --config "web/src/test/auto/e2e/playwright.config.ts"
  fi
  popd
}

builder_run_action test:_all do_browser_tests

function do_test_help() {
  check-markdown  "$KEYMAN_ROOT/web/docs/engine"
}

builder_run_action test:help do_test_help

# Create coverage report
builder_run_action coverage:_all coverage_action

# Start the test server
builder_run_action start node src/tools/testing/test-server/index.cjs
