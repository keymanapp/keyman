#!/usr/bin/env bash
#
# Compile our sourcemap-path remapping module for use by Web builds, releases, etc.
#
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$(dirname "$THIS_SCRIPT")"

################################ Main script ################################

builder_describe "Builds the predictive-text model template implementation module" \
  "@/common/web/keyman-version" \
  "@/common/web/tslib" \
  "@/common/models/wordbreakers" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "--ci"

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
  FLAGS=
  if builder_has_option --ci; then
    FLAGS="-reporter mocha-teamcity-reporter"
  fi

  npm run mocha -- $FLAGS --require test/helpers.js --recursive test

  builder_finish_action success test
fi