#!/usr/bin/env bash
#
# Compile our sourcemap-path remapping module for use by Web builds, releases, etc.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

################################ Main script ################################

builder_describe "Builds the predictive-text wordbreaker implementation module" \
  "@/resources/standards-data/unicode-character-database" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "--ci"

builder_describe_outputs \
  configure          /node_modules \
  build              build/obj/index.js

builder_parse "$@"

function do_configure() {
  verify_npm_setup

  tsc -b src/data-compiler/tsconfig.json
  node ./build/obj/data-compiler/index.js
}

function do_build() {
  tsc -b ./tsconfig.json

  # Declaration bundling.
  tsc -p ./tsconfig.json --emitDeclarationOnly --outFile ./build/lib/index.d.ts
}

function do_test() {
  if builder_has_option --ci; then
    c8 mocha -reporter mocha-teamcity-reporter
  else
    c8 mocha
  fi
}

builder_run_action configure  do_configure
builder_run_action clean      rm -rf build/
builder_run_action build      do_build
builder_run_action test       do_test