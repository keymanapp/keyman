#!/usr/bin/env bash
#
# Compile our sourcemap-path remapping module for use by Web builds, releases, etc.
#
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# This script runs from its own folder
cd "$(dirname "$THIS_SCRIPT")"

################################ Main script ################################

# TODO: for predictive-text, we only need :headless, perhaps we should be splitting modules?
# TODO: remove :tools once kmlmc is a dependency for test:module

builder_describe "Builds the predictive-text wordbreaker implementation module" \
  "clean" \
  "configure" \
  "build" \
  "test"

builder_describe_outputs \
  configure          /node_modules \
  build              build/lib/index.mjs

builder_parse "$@"

### CONFIGURE ACTIONS

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

### CLEAN ACTIONS

if builder_start_action clean; then
  rm -rf build/
  builder_finish_action success clean
fi

### BUILD ACTIONS

if builder_start_action build; then
  npm run tsc -- -b
  node build-bundler.js

  # Declaration bundling.
  npm run tsc -- --emitDeclarationOnly --outFile ./build/lib/index.d.ts

  builder_finish_action success build
fi

if builder_start_action test; then
  npm run mocha

  builder_finish_action success test
fi