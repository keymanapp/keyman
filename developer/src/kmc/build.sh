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
  "@/common/include" \
  "@/common/web/keyman-version" \
  "@/common/web/types" \
  "@/developer/src/kmc-analyze" \
  "@/developer/src/kmc-kmn" \
  "@/developer/src/kmc-ldml" \
  "@/developer/src/kmc-model" \
  "@/developer/src/kmc-model-info" \
  "@/developer/src/kmc-package" \
  "configure                 runs 'npm ci' on root folder" \
  "build                     (default) builds kmc to build/" \
  "clean                     cleans build/ folder" \
  "bundle                    creates a bundled version of kmc" \
  "test                      run automated tests for kmc" \
  "pack                      build a local .tgz pack for testing (note: all npm modules in the repo will be packed by this script)" \
  "publish                   publish to npm (note: all npm modules in the repo will be published by this script)" \
  "--build-path=BUILD_PATH   build directory for bundle" \
  "--dry-run,-n              don't actually publish, just dry run"
builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/kmc/build/src/kmc.js

builder_parse "$@"

if builder_has_option --dry-run; then
  DRY_RUN=--dry-run
else
  DRY_RUN=
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action clean; then
  rm -rf ./build/ ./tsconfig.tsbuildinfo
  builder_finish_action success clean
fi

function copy_schemas() {
  # We need the schema file at runtime and bundled, so always copy it for all actions except `clean`
  local schemas=(
    "$KEYMAN_ROOT/resources/standards-data/ldml-keyboards/techpreview/ldml-keyboard.schema.json"
    "$KEYMAN_ROOT/resources/standards-data/ldml-keyboards/techpreview/ldml-keyboardtest.schema.json"
    "$KEYMAN_ROOT/common/schemas/kvks/kvks.schema.json"
    "$KEYMAN_ROOT/common/schemas/kpj/kpj.schema.json"
    "$KEYMAN_ROOT/common/schemas/kpj-9.0/kpj-9.0.schema.json"
    "$KEYMAN_ROOT/common/schemas/displaymap/displaymap.schema.json"
  )

  mkdir -p "$THIS_SCRIPT_PATH/build/src/util/"
  cp "${schemas[@]}" "$THIS_SCRIPT_PATH/build/src/util/"

  mkdir -p "$THIS_SCRIPT_PATH/build/dist/"
  cp "${schemas[@]}" "$THIS_SCRIPT_PATH/build/dist/"
}

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action configure; then
  copy_schemas
  verify_npm_setup
  builder_finish_action success configure
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action build; then
  copy_schemas
  npm run build
  builder_finish_action success build
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action test; then
  copy_schemas
  npm test
  # TODO: enable c8 (disabled because no coverage at present)
  #     && c8 --reporter=lcov --reporter=text mocha
  builder_finish_action success test
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action bundle; then

  if ! builder_has_option --build-path; then
    builder_finish_action "Parameter --build-path is required" bundle
    exit 64
  fi

  rm -rf build/dist
  mkdir -p build/dist
  node build-bundler.js

  # Manually copy over kmcmplib module and schemas
  copy_schemas
  cp ../kmc-kmn/build/src/import/kmcmplib/wasm-host.wasm build/dist/

  cp build/dist/* "$BUILD_PATH"

  builder_finish_action success bundle
fi

#-------------------------------------------------------------------------------------------------------------------

readonly PACKAGES=(
  common/web/keyman-version
  common/web/types
  common/models/types
  core/include/ldml
  developer/src/kmc-analyze
  developer/src/kmc-kmn
  developer/src/kmc-ldml
  developer/src/kmc-model
  developer/src/kmc-model-info
  developer/src/kmc-package
)

if builder_start_action publish; then
  . "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"

  # For now, kmc will have responsibility for publishing keyman-version and
  # common-types, as well as all the other dependent modules. In the future, we
  # should probably have a top-level npm publish script that publishes all
  # modules for a given release version From: #7595
  for package in "${PACKAGES[@]}"; do
    "$KEYMAN_ROOT/$package/build.sh" publish $DRY_RUN
  done

  # Finally, publish kmc
  builder_publish_to_npm
  builder_finish_action success publish
elif builder_start_action pack; then
  . "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"

  for package in "${PACKAGES[@]}"; do
    "$KEYMAN_ROOT/$package/build.sh" pack $DRY_RUN
  done

  builder_publish_to_pack
  builder_finish_action success pack
fi
