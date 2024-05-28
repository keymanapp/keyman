#!/usr/bin/env bash
#
# Compiles the kmc keyboard compiler.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

builder_describe "Keyman kmc Keyboard Compiler module" \
  "@/common/web/keyman-version" \
  "@/common/web/types" \
  "@/developer/src/kmc-kmn" \
  "@/developer/src/common/web/test-helpers" \
  "configure" \
  "build" \
  "api                       analyze API and prepare API documentation" \
  "clean" \
  "test" \
  "build-fixtures            builds test fixtures for manual examination" \
  "publish                   publish to npm" \
  "--npm-publish+            For publish, do a npm publish, not npm pack (only for CI)" \
  "--dry-run,-n              don't actually publish, just dry run"

builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/kmc-ldml/build/src/main.js \
  api           /developer/build/api/kmc-ldml.api.json

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

if builder_start_action build-fixtures; then
  # Build basic.kmx and emit its checksum
  mkdir -p ./build/test/fixtures
  node ../kmc build ./test/fixtures/basic.xml --no-compiler-version --debug --out-file ./build/test/fixtures/basic-xml.kmx
  printf "${COLOR_GREY}Checksum for basic-xml.kmx: ${COLOR_PURPLE}%s${COLOR_RESET}\n" \
    "$(xxd -g 1 -l 12 ./build/test/fixtures/basic-xml.kmx | cut -d' ' -f 10-13)"

  # Generate a binary file from basic.txt for comparison purposes
  node ../../../common/tools/hextobin/build/hextobin.js ./test/fixtures/basic.txt ./build/test/fixtures/basic-txt.kmx

  builder_finish_action success build-fixtures
fi

builder_run_action api        api-extractor run --local --verbose

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action test; then
  eslint .
  cd test
  tsc -b
  cd ..
  c8 --reporter=lcov --reporter=text mocha "${builder_extra_params[@]}"
  builder_finish_action success test
fi

#-------------------------------------------------------------------------------------------------------------------

. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"

builder_run_action publish     builder_publish_npm
