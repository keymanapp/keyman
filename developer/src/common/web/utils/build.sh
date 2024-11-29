#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"

builder_describe "Build Keyman Developer web utility module" \
  "@/common/web/types" \
  "clean" \
  "configure" \
  "build" \
  "api                       analyze API and prepare API documentation (no-op for now)" \
  "test" \
  publish \
  "--npm-publish+            For publish, do a npm publish, not npm pack (only for CI)" \
  "--dry-run,-n              don't actually publish, just dry run"

builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/common/web/utils/build/src/index.js

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

function copy_cldr_imports() {
  # Store CLDR imports
  # load all versions that have a cldr_info.json
  for CLDR_INFO_PATH in "$KEYMAN_ROOT/resources/standards-data/ldml-keyboards/"*/cldr_info.json
  do
    # TODO-LDML: developer/src/inst/download.in.mak needs these also...
    CLDR_PATH=$(dirname "$CLDR_INFO_PATH")
    CLDR_VER=$(basename "$CLDR_PATH")
    mkdir -p "$THIS_SCRIPT_PATH/build/src/types/import/$CLDR_VER"
    # TODO-LDML: When these are copied, the DOCTYPE will break due to the wrong path. We don't use the DTD so it should be OK.
    cp "$CLDR_INFO_PATH" "$CLDR_PATH/import/"*.xml "$THIS_SCRIPT_PATH/build/src/types/import/$CLDR_VER/"
  done
}

function do_build() {
  copy_cldr_imports
  tsc --build
}

function do_test() {
  local MOCHA_FLAGS=

  if [[ "${TEAMCITY_GIT_PATH:-}" != "" ]]; then
    # we're running in TeamCity
    MOCHA_FLAGS="-reporter mocha-teamcity-reporter"
  fi

  eslint .
  tsc --build test
  readonly C8_THRESHOLD=50
  c8 --reporter=lcov --reporter=text --exclude-after-remap --check-coverage=false --lines $C8_THRESHOLD --statements $C8_THRESHOLD --branches $C8_THRESHOLD --functions $C8_THRESHOLD mocha ${MOCHA_FLAGS}
  builder_echo warning "Coverage thresholds are currently $C8_THRESHOLD%, which is lower than ideal."
  builder_echo warning "Please increase threshold in build.sh as test coverage improves."
}

builder_run_action clean       rm -rf ./build/
builder_run_action configure   verify_npm_setup
builder_run_action build       do_build
builder_run_action test        do_test
builder_run_action publish     builder_publish_npm
