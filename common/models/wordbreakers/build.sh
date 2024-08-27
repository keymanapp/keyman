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

# Note:  the raw text files used for data.inc.ts are found within
# /resources/standards-data/unicode-character-database.
builder_describe "Builds the predictive-text wordbreaker implementation module" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "--ci"

builder_describe_outputs \
  configure          src/main/default/data.inc.ts \
  build              build/main/obj/index.js

builder_parse "$@"

function do_configure() {
  verify_npm_setup

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