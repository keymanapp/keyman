#!/usr/bin/env bash
#
# Compiles the developer tools, including the language model compilers.
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

builder_describe "Build Keyman Keyboard Compiler kmc" \
  "@/common/web/keyman-version" \
  "@/common/web/types" \
  "@../kmc-keyboard" \
  "@../kmc-model" \
  "@../kmc-model-info" \
  "@../kmc-package" \
  "configure                 runs 'npm ci' on root folder" \
  "build                     (default) builds kmc to build/" \
  "clean                     cleans build/ folder" \
  "bundle                    creates a bundled version of kmc" \
  "test                      run automated tests for kmc" \
  "publish                   publish to npm" \
  "--build-path=BUILD_PATH   build directory for bundle" \
  "--dry-run,-n              don't actually publish, just dry run"
builder_describe_outputs \
  configure     /node_modules \
  build         build/src/kmc.js

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action clean; then
  rm -rf ./build/ ./tsconfig.tsbuildinfo
  builder_finish_action success clean
else
  # We need the schema file at runtime and bundled, so always copy it for all actions except `clean`
  mkdir -p "$THIS_SCRIPT_PATH/build/src/util/"
  cp "$KEYMAN_ROOT/resources/standards-data/ldml-keyboards/techpreview/ldml-keyboard.schema.json" "$THIS_SCRIPT_PATH/build/src/util/"
  cp "$KEYMAN_ROOT/resources/standards-data/ldml-keyboards/techpreview/ldml-keyboardtest.schema.json" "$THIS_SCRIPT_PATH/build/src/util/"
  cp "$KEYMAN_ROOT/common/schemas/kvks/kvks.schema.json" "$THIS_SCRIPT_PATH/build/src/util/"
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
  # npm test -- no tests as yet
  builder_finish_action success test
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action bundle; then
  if ! builder_has_option --build-path; then
    builder_finish_action "Parameter --build-path is required" bundle
    exit 64
  fi

  mkdir -p build/cjs-src
  npm run bundle
  cp build/cjs-src/* "$BUILD_PATH"

  builder_finish_action success bundle
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action publish; then
  . "$KEYMAN_ROOT/resources/build/npm-publish.inc.sh"
  npm_publish

  # For now, kmc will have responsibility for publishing keyman-version. In
  # the future, we should probably have a top-level npm publish script that
  # publishes all modules for a given release version
  # From: #7595
  "$KEYMAN_ROOT/common/web/keyman-version/build.sh" publish $DRY_RUN

  builder_finish_action success publish
fi
