#!/usr/bin/env bash
#
# Compiles the Language Modeling Layer for common use in predictive text and autocorrective applications.
# Designed for optimal compatibility with the Keyman Suite.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

WORKER_OUTPUT=build/obj
WORKER_OUTPUT_FILENAME=build/lib/worker-main.js

INTERMEDIATE=./build/intermediate
LIB=./build/lib

bundle_cmd="node ${KEYMAN_ROOT}/web/src/tools/es-bundling/build/common-bundle.mjs"

SRCMAP_CLEANER="node $KEYMAN_ROOT/web/build/tools/building/sourcemap-root/index.js"

################################ Main script ################################

builder_describe \
  "Compiles the Language Modeling Layer for common use in predictive text and autocorrective applications." \
  "@/common/web/keyman-version" \
  "@/web/src/tools/building/sourcemap-root" \
  "@/web/src/tools/es-bundling" \
  "@../wordbreakers" \
  "@../templates" \
  configure clean build test --ci

builder_describe_outputs \
  configure     /node_modules \
  build         "/web/src/engine/predictive-text/worker-thread/${LIB}/worker-main.wrapped.min.js"

builder_parse "$@"

function do_configure() {
  verify_npm_setup

  # Configure Web browser-engine testing environments.  As is, this should only
  # make changes when we update the dependency, even on our CI build agents.
  playwright install
}

function do_build() {
  EXT_FLAGS=
  if builder_has_option --ci; then
    EXT_FLAGS=--ci
  fi

  # Declaration bundling.
  tsc --emitDeclarationOnly --outFile $INTERMEDIATE/worker-main.d.ts

  # Some automated tests currently rely upon the individual output files.
  tsc

  mkdir -p $LIB


  # The ES6 target needs no polyfills - we go straight to the wrapped version.
  $bundle_cmd src/main/worker-main.ts \
    --out $INTERMEDIATE/worker-main.js \
    --charset "utf8" \
    --target "es6" \
    --sourceRoot '@keymanapp/keyman/web/src/engine/predictive-text/worker-thread/src/main'

  $SRCMAP_CLEANER \
    $INTERMEDIATE/worker-main.js.map \
    $INTERMEDIATE/worker-main.js.map \
    --clean

  $bundle_cmd src/main/worker-main.ts \
    --out $INTERMEDIATE/worker-main.min.js \
    --minify \
    --charset "utf8" \
    --profile build/filesize-profile.log \
    --target "es6" \
    --sourceRoot '@keymanapp/keyman/web/src/engine/predictive-text/worker-thread/src/main'

  $SRCMAP_CLEANER \
    $INTERMEDIATE/worker-main.min.js.map \
    $INTERMEDIATE/worker-main.min.js.map \
    --clean

  node build-wrapper.js $INTERMEDIATE/worker-main.js \
    --out $LIB/worker-main.wrapped.js \
    --sourceMap
  node build-wrapper.js $INTERMEDIATE/worker-main.min.js \
    --out $LIB/worker-main.wrapped.min.js
}

function do_test() {
  local MOCHA_FLAGS=
  local WTR_CONFIG=
  local WTR_DEBUG=

  if builder_has_option --ci; then
    MOCHA_FLAGS="$MOCHA_FLAGS --reporter mocha-teamcity-reporter"
    WTR_CONFIG=.CI
  fi

  if builder_has_option --debug; then
    WTR_DEBUG=" --manual"
  fi

  c8 mocha --recursive $MOCHA_FLAGS ./src/tests/mocha/cases/

  web-test-runner --config ./src/tests/test-runner/web-test-runner${WTR_CONFIG}.config.mjs ${WTR_DEBUG}
}

builder_run_action configure  do_configure
builder_run_action clean      rm -rf build/ intermediate/
builder_run_action build      do_build
builder_run_action test       do_test
