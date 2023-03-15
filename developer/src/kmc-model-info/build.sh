#!/usr/bin/env bash
#
# Compiles the kmc lexical model model-info compiler.
#

# Exit on command failure and when using unset variables:
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

cd "$THIS_SCRIPT_PATH"

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

builder_describe "Build Keyman kmc Lexical Model model-info Compiler module" \
  "@/developer/src/kmc-package" \
  "configure" \
  "build" \
  "clean" \
  "test" \
  "publish                   publish to npm" \
  "--dry-run,-n              don't actually publish, just dry run"
builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/kmc-model-info/build/src/model-info-compiler.js

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action clean; then
  rm -rf ./build/ ./tsconfig.tsbuildinfo
  builder_finish_action success clean
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action build; then
  npm run build
  builder_finish_action success build
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action test; then
  #npm test - no tests as yet
  builder_finish_action success test
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action publish; then
  . "$KEYMAN_ROOT/resources/build/npm-publish.inc.sh"
  npm_publish
  builder_finish_action success publish
fi
