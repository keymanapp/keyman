#!/usr/bin/env bash
#
# Compile our sourcemap-path remapping module for use by Web builds, releases, etc.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"
. "$KEYMAN_ROOT/web/common.inc.sh"

################################ Main script ################################

SUBPROJECT_NAME=engine/predictive-text/wordbreakers

# Note:  the raw text files used for data.inc.ts are found within
# /resources/standards-data/unicode-character-database.
builder_describe "Builds the predictive-text wordbreaker implementation module" \
  "@../templates test" \
  "clean" \
  "configure" \
  "build" \
  "test"

builder_describe_outputs \
  configure          src/main/default/data.inc.ts \
  build              build/main/obj/index.js

builder_parse "$@"

function do_configure() {
  node_select_version_and_npm_ci

  # This is a script used to build the data.inc.ts file needed by the
  # default wordbreaker.  We rarely update the backing data, but it
  # is needed _before_ the `build` action's compilation step.
  tsc -b tools/data-compiler/tsconfig.json
  node ./build/tools/data-compiler/obj/index.js
}

function do_build() {
  tsc -b ./tsconfig.json

  # Declaration bundling.
  tsc -p ./tsconfig.json --emitDeclarationOnly --outFile ./build/main/lib/index.d.ts
}

function do_test() {
  local FLAGS=()

  if builder_is_running_on_teamcity; then
    FLAGS+=(--reporter "${KEYMAN_ROOT}/common/test/resources/mocha-teamcity-reporter/teamcity.cjs" --reporter-options parentFlowId="unit_tests")
    echo "##teamcity[flowStarted flowId='unit_tests']"
  fi

  c8 mocha "${FLAGS[@]}" tests

  if builder_is_running_on_teamcity; then
    # we're running in TeamCity
    echo "##teamcity[flowFinished flowId='unit_tests']"
  fi
}

builder_run_action configure  do_configure
builder_run_action clean      rm -rf build/
builder_run_action build      do_build
builder_run_action test       test-headless-typescript $SUBPROJECT_NAME