#!/usr/bin/env bash
#
# Compiles the Language Modeling Layer for common use in predictive text and autocorrective applications.
# Designed for optimal compatibility with the Keyman Suite.
#

# Exit on command failure and when using unset variables:
set -eu

# Include some helper functions from resources

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

WORKER_OUTPUT=build/obj
WORKER_OUTPUT_FILENAME=build/lib/worker-main.js

################################ Main script ################################

builder_describe \
  "Compiles the Language Modeling Layer for common use in predictive text and autocorrective applications." \
  "@/common/web/keyman-version" \
  "@/common/web/es-bundling" \
  "@/common/models/wordbreakers" \
  "@/common/models/templates" \
  configure clean build test --ci

builder_describe_outputs \
  configure     /node_modules \
  build         /common/web/lm-worker/build/lib/worker-main.wrapped.min.js

builder_parse "$@"

function do_build() {
  # Build worker with tsc first
  tsc -b $builder_verbose || builder_die "Could not build worker."
  node build-bundler.js

  EXT_FLAGS=
  if builder_has_option --ci; then
    EXT_FLAGS=--ci
  fi

  # Declaration bundling.
  tsc --emitDeclarationOnly --outFile ./build/lib/index.d.ts
  tsc --emitDeclarationOnly --outFile ./build/lib/worker-main.d.ts

  echo "Preparing the polyfills + worker for script-embedding"
  node build-polyfill-concatenator.js

  node build-wrap-and-minify.js --debug
  node build-wrap-and-minify.js --minify
}

function do_test() {
  local MOCHA_FLAGS=

  if builder_has_option --ci; then
    MOCHA_FLAGS="$MOCHA_FLAGS --reporter mocha-teamcity-reporter"
  fi

  c8 mocha --recursive $MOCHA_FLAGS ./src/test/cases/
}

builder_run_action configure  verify_npm_setup
builder_run_action clean      rm -rf build/
builder_run_action build      do_build
builder_run_action test       do_test