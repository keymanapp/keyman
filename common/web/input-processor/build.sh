#!/usr/bin/env bash
#
# Compile KeymanWeb's 'keyboard-processor' module, one of the components of Web's 'core' module.
#
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

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
  tsc -b ./tsconfig.json
  node build-bundler.js

  # Declaration bundling.
  tsc --emitDeclarationOnly --outFile ./build/lib/index.d.ts

  builder_finish_action success build
fi

# TEST ACTIONS

if builder_start_action test; then
  FLAGS=
  if builder_has_option --ci; then
    FLAGS="--reporter mocha-teamcity-reporter"
  fi

  mocha --recursive $FLAGS ./tests/cases/

  builder_finish_action success test
fi