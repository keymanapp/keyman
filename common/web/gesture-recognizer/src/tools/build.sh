#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

BUNDLE_CMD="node $KEYMAN_ROOT/common/web/es-bundling/build/common-bundle.mjs"

################################ Main script ################################

builder_describe "Testing-oriented tools for the Gesture Recognizer module of web-based Keyman OSKs" \
  "clean" \
  "build" \
  ":fixture       The HTML-element fixture and CSS fixture used for both user-testing and unit-testing" \
  ":recorder      The web page used for recording input sequences for use in unit-testing" \
  ":test-module   The TS library used to interface with the main gesture-recognizer module for tests"

builder_parse "$@"

builder_describe_outputs \
  configure            /node_modules \
  build:fixture        /common/web/gesture-recognizer/build/tools/host-fixture.html \
  build:recorder       /common/web/gesture-recognizer/src/tools/recorder/build/recorder.mjs \
  build:test-module    /common/web/gesture-recognizer/build/tools/lib/index.mjs

# TODO: build if out-of-date if test is specified
# TODO: configure if npm has not been run, and build is specified

if builder_start_action clean:recorder; then
  rm -rf ./recorder/build
  builder_finish_action success clean:recorder
fi

if builder_start_action clean:fixture; then
  rm -f ../../build/tools/host-fixture.html
  rm -f ../../build/tools/gestureHost.css
  builder_finish_action success clean:fixture
fi

if builder_start_action clean:test-module; then
  rm -rf ../../build/tools/*.ts*
  rm -rf ../../build/tools/*.js*
  builder_finish_action success clean:test-module
fi

if builder_start_action build:fixture; then
  if [ ! -d ../../build/tools ]; then
    mkdir -p ../../build/tools
  fi
  ./host-fixture/extract-fixture.sh > ../../build/tools/host-fixture.html
  cp ./host-fixture/gestureHost.css ../../build/tools/gestureHost.css
  builder_finish_action success build:fixture
fi

if builder_start_action build:recorder; then
  if [ ! -d recorder/build ]; then
    mkdir -p recorder/build
  fi
  cp recorder/src/pageStyle.css    recorder/build/pageStyle.css
  cp recorder/src/recorder.mjs     recorder/build/recorder.mjs

  cp host-fixture/gestureHost.css  recorder/build/gestureHost.css

  # Thanks to https://stackoverflow.com/a/10107668 for this tidbit.
  # Searches for FIXTURE_TARGET above and replaces it with the actual fixture!
  pushd recorder >/dev/null
  node update-index.cjs build/index.html
  popd >/dev/null
  builder_finish_action success build:recorder
fi

if builder_start_action build:test-module; then
  tsc -b "$THIS_SCRIPT_PATH/unit-test-resources/tsconfig.json"

  $BUNDLE_CMD    "${KEYMAN_ROOT}/common/web/gesture-recognizer/build/tools/obj/index.js" \
    --out        "${KEYMAN_ROOT}/common/web/gesture-recognizer/build/tools/lib/index.mjs" \
    --format "esm"

  builder_finish_action success build:test-module
fi