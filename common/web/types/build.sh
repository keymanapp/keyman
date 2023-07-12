#!/usr/bin/env bash
#
# Compiles the common file types module
#

# Exit on command failure and when using unset variables:
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

cd "$THIS_SCRIPT_PATH"

builder_describe "Build Keyman common file types module" \
  "@/common/web/keyman-version" \
  "configure" \
  "build" \
  "clean" \
  "test" \
  "pack                      build a local .tgz pack for testing" \
  "publish                   publish to npm" \
  "--dry-run,-n              don't actually publish, just dry run"
builder_describe_outputs \
  configure   /node_modules \
  build       /common/web/types/build/src/main.js

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action clean; then
  rm -rf ./build/ ./tsconfig.tsbuildinfo
  builder_finish_action success clean
fi

function copy_schemas() {
  # We need the schema files at runtime and bundled, so always copy it for all actions except `clean`

  # We need the schema file at runtime and bundled, so always copy it for all actions except `clean`
  local schemas=(
    "$KEYMAN_ROOT/resources/standards-data/ldml-keyboards/techpreview/ldml-keyboard.schema.json"
    "$KEYMAN_ROOT/resources/standards-data/ldml-keyboards/techpreview/ldml-keyboardtest.schema.json"
    "$KEYMAN_ROOT/common/schemas/kvks/kvks.schema.json"
    "$KEYMAN_ROOT/common/schemas/kpj/kpj.schema.json"
    "$KEYMAN_ROOT/common/schemas/kpj-9.0/kpj-9.0.schema.json"
    "$KEYMAN_ROOT/common/schemas/displaymap/displaymap.schema.json"
  )

  rm -rf "$THIS_SCRIPT_PATH/src/schemas"
  mkdir -p "$THIS_SCRIPT_PATH/src/schemas"
  cp "${schemas[@]}" "$THIS_SCRIPT_PATH/src/schemas/"
  # Store CLDR imports
  # load all versions that have a cldr_info.json
  for CLDR_INFO_PATH in "$KEYMAN_ROOT/resources/standards-data/ldml-keyboards/"*/cldr_info.json
  do
    # TODO-LDML: developer/src/inst/download.in.mak needs these also...
    CLDR_PATH=$(dirname "$CLDR_INFO_PATH")
    CLDR_VER=$(basename "$CLDR_PATH")
    mkdir -p "$THIS_SCRIPT_PATH/build/src/import/$CLDR_VER"
    # TODO-LDML: When these are copied, the DOCTYPE will break due to the wrong path. We don't use the DTD so it should be OK.
    cp "$CLDR_INFO_PATH" "$CLDR_PATH/import/"*.xml "$THIS_SCRIPT_PATH/build/src/import/$CLDR_VER/"
  done
}

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action configure; then
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
  npm test -- "${builder_extra_params[@]}"
  builder_finish_action success test
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action publish; then
  copy_schemas
  . "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"
  builder_publish_to_npm
  builder_finish_action success publish
elif builder_start_action pack; then
  copy_schemas
  . "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"
  builder_publish_to_pack
  builder_finish_action success pack
fi
