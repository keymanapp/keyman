#!/usr/bin/env bash
#
# Compile our sourcemap-path remapping module for use by Web builds, releases, etc.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"

################################ Main script ################################

builder_describe "Builds the predictive-text model template implementation module" \
  "@/common/web/keyman-version" \
  "@/web/src/tools/es-bundling" \
  "@../wordbreakers" \
  "clean" \
  "configure" \
  "build" \
  "test"

builder_describe_outputs \
  configure          /node_modules \
  build              build/obj/index.js

builder_parse "$@"


function do_build() {
  tsc -b

  # Declaration bundling.
  tsc --emitDeclarationOnly --outFile ./build/lib/index.d.ts
}

function do_test() {
  local FLAGS=
  if builder_is_ci_build; then
    FLAGS="-reporter mocha-teamcity-reporter"
  fi

  c8 mocha $FLAGS --require tests/helpers.js --recursive tests
}

builder_run_action configure  verify_npm_setup
builder_run_action clean      rm -rf build/
builder_run_action build      do_build
builder_run_action test       do_test