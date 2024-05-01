#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

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
  configure   /common/web/types/src/schemas/kpj.schema.ts \
  build       /common/web/types/build/src/main.js

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

function compile_schemas() {
  # We need the schema files at runtime and bundled, so always copy it for all actions except `clean`
  local schemas=(
    "$KEYMAN_ROOT/resources/standards-data/ldml-keyboards/45/ldml-keyboard3.schema.json"
    "$KEYMAN_ROOT/resources/standards-data/ldml-keyboards/45/ldml-keyboardtest3.schema.json"
    "$KEYMAN_ROOT/common/schemas/kvks/kvks.schema.json"
    "$KEYMAN_ROOT/common/schemas/kpj/kpj.schema.json"
    "$KEYMAN_ROOT/common/schemas/kpj-9.0/kpj-9.0.schema.json"
    "$KEYMAN_ROOT/common/schemas/displaymap/displaymap.schema.json"
    "$KEYMAN_ROOT/common/schemas/keyman-touch-layout/keyman-touch-layout.spec.json"
    "$KEYMAN_ROOT/common/schemas/keyman-touch-layout/keyman-touch-layout.clean.spec.json"
    "$KEYMAN_ROOT/common/schemas/keyboard_info/keyboard_info.schema.json"
    "$KEYMAN_ROOT/common/schemas/kmp/kmp.schema.json"
  )

  rm -rf "$THIS_SCRIPT_PATH/obj/schemas"
  mkdir -p "$THIS_SCRIPT_PATH/obj/schemas"
  rm -rf "$THIS_SCRIPT_PATH/src/schemas"
  mkdir -p "$THIS_SCRIPT_PATH/src/schemas"
  cp "${schemas[@]}" "$THIS_SCRIPT_PATH/src/schemas/"

  # TODO: use https://github.com/tc39/proposal-json-modules instead of this once it stablises
  for schema in "${schemas[@]}"; do
    local schema_base="$(basename "$schema" .json)"
    local fn="$THIS_SCRIPT_PATH/src/schemas/$schema_base"
    local out="$THIS_SCRIPT_PATH/obj/schemas/$schema_base.validator.cjs"

    # emit a .ts wrapper for the schema file

    builder_echo "Compiling schema $schema_base.json"
    echo 'export default ' > "$fn.ts"
    cat "$fn.json" >> "$fn.ts"

    # emit a compiled validator for the schema file

    # While would seem obvious to just run 'ajv' directly here, somewhere node
    # is picking up the wrong path for the build and breaking the formats
    # imports. So it is essential to use `npm run` at this point, even though it
    # is painfully slower, at least until we figure out the path discrepancy.
    npm run build:schema -- -c ./tools/formats.cjs -s "$fn.json" --strict-types false -o "$out"
  done

  # the validators now need to be compiled to esm
  node tools/schema-bundler.js
}

function copy_cldr_imports() {
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

function do_configure() {
  compile_schemas
  copy_cldr_imports
  verify_npm_setup
}

function do_test() {
  eslint .
  tsc --build test
  c8 --skip-full --reporter=lcov --reporter=text mocha "${builder_extra_params[@]}"
}

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean      rm -rf ./build/ ./tsconfig.tsbuildinfo
builder_run_action configure  do_configure
builder_run_action build      tsc --build
builder_run_action test       do_test

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action publish; then
  . "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"
  builder_publish_to_npm
  builder_finish_action success publish
elif builder_start_action pack; then
  . "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"
  builder_publish_to_pack
  builder_finish_action success pack
fi
