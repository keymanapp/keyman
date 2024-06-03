#!/usr/bin/env bash
#
# Compiles the developer tools, including the language model compilers.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/developer/src/packages.inc.sh"

builder_describe "Build Keyman Keyboard Compiler kmc" \
  "@/common/include" \
  "@/common/web/keyman-version" \
  "@/common/web/types" \
  "@/developer/src/common/web/utils" \
  "@/developer/src/kmc-analyze" \
  "@/developer/src/kmc-keyboard-info" \
  "@/developer/src/kmc-kmn" \
  "@/developer/src/kmc-ldml" \
  "@/developer/src/kmc-model" \
  "@/developer/src/kmc-model-info" \
  "@/developer/src/kmc-package" \
  "configure                 runs 'npm ci' on root folder" \
  "build                     (default) builds kmc to build/" \
  "clean                     cleans build/ folder" \
  "bundle                    creates a bundled version of kmc" \
  "api                       prepare compiler error documentation" \
  "test                      run automated tests for kmc" \
  publish \
  "--build-path=BUILD_PATH   build directory for bundle" \
  "--npm-publish+            For publish, do a npm publish, not npm pack (only for CI)" \
  "--dry-run,-n              don't actually publish, just dry run"

builder_describe_outputs \
  configure     /node_modules \
  build         /developer/src/kmc/build/src/kmc.js

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

function do_build() {
  tsc --build
  cp "$KEYMAN_ROOT/resources/standards-data/ldml-keyboards/unicode-license.txt" ./build/unicode-license.txt
}

#-------------------------------------------------------------------------------------------------------------------

function do_api() {
  rm -rf ./build/messages
  mkdir -p ./build/messages
  node . message --format markdown --out-path build/messages
}

#-------------------------------------------------------------------------------------------------------------------

function do_test() {
  eslint .
  tsc --build test/
  readonly C8_THRESHOLD=48
  c8 --reporter=lcov --reporter=text --lines $C8_THRESHOLD --statements $C8_THRESHOLD --branches $C8_THRESHOLD --functions $C8_THRESHOLD mocha
  builder_echo warning "Coverage thresholds are currently $C8_THRESHOLD%, which is lower than ideal."
  builder_echo warning "Please increase threshold in build.sh as test coverage improves."
}

#-------------------------------------------------------------------------------------------------------------------

function do_bundle() {
  SOURCEMAP_PATHS=( "${PACKAGES[@]}" )
  SOURCEMAP_PATHS=( "${SOURCEMAP_PATHS[@]/%//build}" )
  SOURCEMAP_PATHS=( "${SOURCEMAP_PATHS[@]/#/../../../}" )
  readonly SOURCEMAP_PATHS

  if ! builder_has_option --build-path; then
    builder_finish_action "Parameter --build-path is required" bundle
    exit 64
  fi

  rm -rf build/dist

  mkdir -p build/dist
  node build-bundler.js

  sentry-cli sourcemaps inject \
    --org keyman \
    --project keyman-developer \
    --release "$VERSION_GIT_TAG"  \
    build/dist/ "${SOURCEMAP_PATHS[@]}"

  # Manually copy over kmcmplib module
  cp ../kmc-kmn/build/src/import/kmcmplib/wasm-host.wasm build/dist/

  cp build/dist/* "$BUILD_PATH"

  builder_finish_action success bundle
}

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean      rm -rf ./build/ ./tsconfig.tsbuildinfo
builder_run_action configure  verify_npm_setup
builder_run_action build      do_build
builder_run_action api        do_api
builder_run_action bundle     do_bundle
builder_run_action publish    builder_publish_npm
