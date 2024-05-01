#!/usr/bin/env bash
#
# Compile KeymanWeb's 'keyboard-processor' module, one of the components of Web's 'core' module.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

BUNDLE_CMD="node $KEYMAN_ROOT/common/web/es-bundling/build/common-bundle.mjs"

################################ Main script ################################

builder_describe "Builds the standalone, headless form of Keyman Engine for Web's input-processor module" \
  "@/common/web/keyman-version" \
  "@/common/web/keyboard-processor" \
  "@/common/predictive-text" \
  "@/developer/src/kmc-model test" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "--ci        Sets $(builder_term test) action to use CI-based test configurations & reporting"

builder_describe_outputs \
  configure          /node_modules \
  build              /common/web/input-processor/build/lib/index.mjs \

builder_parse "$@"

function do_build() {
  tsc -b ./tsconfig.json

  $BUNDLE_CMD    "${KEYMAN_ROOT}/common/web/input-processor/build/obj/index.js" \
    --out        "${KEYMAN_ROOT}/common/web/input-processor/build/lib/index.mjs" \
    --format esm

  # Declaration bundling.
  tsc --emitDeclarationOnly --outFile ./build/lib/index.d.ts
}

function do_test() {
  local FLAGS=
  if builder_has_option --ci; then
    FLAGS="--reporter mocha-teamcity-reporter"
  fi

  mocha --recursive $FLAGS ./tests/cases/
}

builder_run_action configure  verify_npm_setup
builder_run_action clean      rm -rf build/
builder_run_action build      do_build
builder_run_action test       do_test