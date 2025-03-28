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

################################ Main script ################################

builder_describe "Builds the predictive-text model template implementation module" \
  "@/common/web/keyman-version" \
  "@/web/src/tools/es-bundling" \
  "@../wordbreakers" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "--ci"

builder_describe_outputs \
  configure          /node_modules \
  build              build/obj/index.js

builder_parse "$@"


function do_build() {
  tsc -b

  # Declaration bundling.
  tsc --emitDeclarationOnly --outFile ./build/lib/index.d.ts

  # Bundler definition
  BUNDLE_CMD="node ${KEYMAN_ROOT}/web/src/tools/es-bundling/build/common-bundle.mjs"

  # Bundles and minifies the back-compatibility trie-decoding functionality for inclusion in compiled models
  # (Still needs a bit of rework to get it _just_ right, though.)
  #
  # May want to use the `global_name` esbuild setting to allow a directly-embeddable export, though that
  # may in itself add extra overhead
  $BUNDLE_CMD "${KEYMAN_ROOT}/web/src/engine/predictive-text/templates/src/decompressor-main.js" \
    --out        "${KEYMAN_ROOT}/web/src/engine/predictive-text/templates/build/lib/decompressor.js" \
    --charset    "utf8" \
    --format     "iife" \
    --globalName "triecompat" \
    --sourceRoot "@keymanapp/keyman/web/src/engine/predictive-text/templates/build/lib" \
    --target     "es6" \
    --minify
}

function do_test() {
  local FLAGS=
  if builder_has_option --ci; then
    FLAGS="-reporter mocha-teamcity-reporter"
  fi

  c8 mocha $FLAGS --require tests/helpers.js --recursive tests
}

builder_run_action configure  verify_npm_setup
builder_run_action clean      rm -rf build/
builder_run_action build      do_build
builder_run_action test       do_test