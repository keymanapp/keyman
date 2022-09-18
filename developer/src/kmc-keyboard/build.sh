#!/usr/bin/env bash
#
# Compiles the kmc keyboard compiler.
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

builder_describe "Build Keyman kmc Keyboard Compiler module" \
  "configure" \
  "build" \
  "clean" \
  "test" \
  "build-fixtures            builds test fixtures for manual examination" \
  "publish                   publish to npm" \
  "--dry-run,-n              don't actually publish, just dry run"

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

if builder_has_action clean; then
  rm -rf ./build/ ./tsconfig.tsbuildinfo
  builder_report success clean
else
  # We need the schema file at runtime and bundled, so always copy it for all actions except `clean`
  mkdir -p "$THIS_SCRIPT_PATH/build/src/"
  cp "$KEYMAN_ROOT/resources/standards-data/ldml-keyboards/techpreview/ldml-keyboard.schema.json" "$THIS_SCRIPT_PATH/build/src/"
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_has_action configure; then
  verify_npm_setup
  builder_report success configure
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_has_action build; then
  npm run build
  builder_report success build
fi

#-------------------------------------------------------------------------------------------------------------------

if builder_has_action build-fixtures; then
  # Build basic.kmx and emit its checksum
  mkdir -p ./build/test/fixtures
  node . ./test/fixtures/basic.xml --no-compiler-version --debug --out-file ./build/test/fixtures/basic-xml.kmx
  printf "${COLOR_GREY}Checksum for basic-xml.kmx: ${COLOR_PURPLE}%s${COLOR_RESET}\n" \
    "$(xxd -g 1 -l 12 ./build/test/fixtures/basic-xml.kmx | cut -d' ' -f 10-13)"

  # Generate a binary file from basic.txt for comparison purposes
  node ../../../common/tools/hextobin/build/hextobin.js ./test/fixtures/basic.txt ./build/test/fixtures/basic-txt.kmx

  builder_report success build-fixtures
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
