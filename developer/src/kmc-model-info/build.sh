#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

builder_describe "Build Keyman kmc Lexical Model model-info Compiler module" \
  "@/common/web/types" \
  "clean" \
  "configure" \
  "build" \
  "api                       analyze API and prepare API documentation" \
  "test" \
  "publish                   publish to npm" \
  "--npm-publish+            For publish, do a npm publish, not npm pack (only for CI)" \
  "--dry-run,-n              don't actually publish, just dry run"

builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/kmc-model-info/build/src/model-info-compiler.js \
  api           /developer/build/api/kmc-model-info.api.json

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

builder_run_action api        api-extractor run --local --verbose

#-------------------------------------------------------------------------------------------------------------------

function do_test() {
  local MOCHA_FLAGS=

  if [[ "${TEAMCITY_GIT_PATH:-}" != "" ]]; then
    # we're running in TeamCity
    MOCHA_FLAGS="-reporter mocha-teamcity-reporter"
  fi
  eslint .
  tsc --build test
  c8 --reporter=lcov --reporter=text --exclude-after-remap --check-coverage=false --lines 80 mocha ${MOCHA_FLAGS}
  # TODO: remove --lines 80 and improve coverage
}

builder_run_action test    do_test

#-------------------------------------------------------------------------------------------------------------------

. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"

builder_run_action publish     builder_publish_npm
