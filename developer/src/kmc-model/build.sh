#!/usr/bin/env bash
#
# Compiles the kmc lexical model compiler.
#

# Exit on command failure and when using unset variables:
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

cd "$THIS_SCRIPT_PATH"

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

builder_describe "Build Keyman kmc Lexical Model Compiler module" \
  "configure" \
  "build" \
  "clean" \
  "test" \
  "publish                   publish to npm" \
  "--dry-run,-n              don't actually publish, just dry run"

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

if builder_has_action clean; then
  rm -rf ./build/ ./tsconfig.tsbuildinfo
  builder_report success clean
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_has_action configure; then
  verify_npm_setup
  builder_report success configure
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_has_action build; then
  # Note: build-cjs only emits lexical-model-compiler.cjs at this time, as that
  # is the only file required by other non-ES modules
  # (common/web/input-processor tests)
  mkdir -p build/cjs-src
  npm run build
  builder_report success build
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_has_action test; then
  npm test
  builder_report success test
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_has_action publish; then
  . "$KEYMAN_ROOT/resources/build/npm-publish.inc.sh"
  npm_publish
  builder_report success publish
fi
