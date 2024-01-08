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

INTERMEDIATE=./build/intermediate
LIB=./build/lib

bundle_cmd="node ../es-bundling/build/common-bundle.mjs"

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
  build         /common/web/lm-worker/$LIB/worker-main.wrapped.min.js

builder_parse "$@"

function do_build() {
  # Build worker with tsc first
  tsc -b $builder_verbose || builder_die "Could not build worker."

  $bundle_cmd build/obj/worker-main.js \
    --out $INTERMEDIATE/worker-main.es5.js

  $bundle_cmd build/obj/worker-main.js \
    --out $INTERMEDIATE/worker-main.min.es5.js \
    --minify \
    --profile build/filesize-profile.es5.log

  EXT_FLAGS=
  if builder_has_option --ci; then
    EXT_FLAGS=--ci
  fi

  # Declaration bundling.
  tsc --emitDeclarationOnly --outFile $INTERMEDIATE/worker-main.d.ts

  echo "Preparing the polyfills + worker for script-embedding"
  node build-polyfiller.js $INTERMEDIATE/worker-main.es5.js \
    --out $INTERMEDIATE/worker-main.polyfilled.es5.js

  mkdir -p $LIB
  node build-wrapper.js $INTERMEDIATE/worker-main.polyfilled.es5.js \
    --out $LIB/worker-main.wrapped.es5.js \
    --sourceMap
  node build-wrapper.js $INTERMEDIATE/worker-main.polyfilled.es5.min.js \
    --out $LIB/worker-main.wrapped.es5.min.js


  # The ES6 target needs no polyfills - we go straight to the wrapped version.
  $bundle_cmd src/main/worker-main.ts \
    --out $INTERMEDIATE/worker-main.js \
    --target "es6"

  $bundle_cmd src/main/worker-main.ts \
    --out $INTERMEDIATE/worker-main.min.js \
    --minify \
    --profile build/filesize-profile.log \
    --target "es6"

  node build-wrapper.js $INTERMEDIATE/worker-main.js \
    --out $LIB/worker-main.wrapped.js \
    --sourceMap
  node build-wrapper.js $INTERMEDIATE/worker-main.min.js \
    --out $LIB/worker-main.wrapped.min.js
}

function do_test() {
  local MOCHA_FLAGS=

  if builder_has_option --ci; then
    MOCHA_FLAGS="$MOCHA_FLAGS --reporter mocha-teamcity-reporter"
  fi

  c8 mocha --recursive $MOCHA_FLAGS ./src/test/cases/
}

builder_run_action configure  verify_npm_setup
builder_run_action clean      rm -rf build/ intermediate/
builder_run_action build      do_build
builder_run_action test       do_test