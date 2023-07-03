#!/usr/bin/env bash
#
# Compiles the kmc keyboard compiler.
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

builder_describe "Build Keyman kmc Keyboard Compiler module" \
  "@/common/web/keyman-version" \
  "@/common/web/types" \
  "@/developer/src/kmc-kmn" \
  "@/developer/src/common/web/test-helpers" \
  "configure" \
  "build" \
  "clean" \
  "test" \
  "build-fixtures            builds test fixtures for manual examination" \
  "pack                      build a local .tgz pack for testing" \
  "publish                   publish to npm" \
  "--dry-run,-n              don't actually publish, just dry run"
builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/kmc-ldml/build/src/main.js

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action clean; then
  rm -rf ./build/ ./tsconfig.tsbuildinfo
  builder_finish_action success clean
fi

SCHEMAS_COPIED=false

copy_schemas() {
  # TODO: why do we need a copy of the schemas here? kmc already has a copy of
  #       them; are they used only by tests, in which case, can't we use them in
  #       their source location?
  if $SCHEMAS_COPIED; then
    return 0
  fi
  SCHEMAS_COPIED=true
  # We need the schema file at runtime and bundled, so always copy it for all actions except `clean`
  mkdir -p "$THIS_SCRIPT_PATH/build/src/"
  cp "$KEYMAN_ROOT/resources/standards-data/ldml-keyboards/techpreview/ldml-keyboard.schema.json" "$THIS_SCRIPT_PATH/build/src/"
  cp "$KEYMAN_ROOT/resources/standards-data/ldml-keyboards/techpreview/ldml-keyboardtest.schema.json" "$THIS_SCRIPT_PATH/build/src/"
  cp "$KEYMAN_ROOT/common/schemas/kvks/kvks.schema.json" "$THIS_SCRIPT_PATH/build/src/"
  cp "$KEYMAN_ROOT/common/schemas/kpj/kpj.schema.json" "$THIS_SCRIPT_PATH/build/src/"
  cp "$KEYMAN_ROOT/common/schemas/kpj-9.0/kpj-9.0.schema.json" "$THIS_SCRIPT_PATH/build/src/"
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

if builder_start_action build-fixtures; then
  copy_schemas

  # Build basic.kmx and emit its checksum
  mkdir -p ./build/test/fixtures
  node ../kmc build ./test/fixtures/basic.xml --no-compiler-version --debug --out-file ./build/test/fixtures/basic-xml.kmx
  printf "${COLOR_GREY}Checksum for basic-xml.kmx: ${COLOR_PURPLE}%s${COLOR_RESET}\n" \
    "$(xxd -g 1 -l 12 ./build/test/fixtures/basic-xml.kmx | cut -d' ' -f 10-13)"

  # Generate a binary file from basic.txt for comparison purposes
  node ../../../common/tools/hextobin/build/hextobin.js ./test/fixtures/basic.txt ./build/test/fixtures/basic-txt.kmx

  builder_finish_action success build-fixtures
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action test; then
  copy_schemas
  npm test
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
