#!/usr/bin/env bash
#
# Compiles the developer tools, including the language model compilers.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"
. "$KEYMAN_ROOT/resources/build/typescript.inc.sh"
. "$KEYMAN_ROOT/resources/build/ci/npm-packages.inc.sh"

builder_describe "Build Keyman Keyboard Compiler kmc" \
  "@/common/include" \
  "@/common/web/keyman-version" \
  "@/common/web/types" \
  "@/core/include/ldml" \
  "@/developer/src/common/web/utils" \
  "@/developer/src/kmc-analyze" \
  "@/developer/src/kmc-copy" \
  "@/developer/src/kmc-convert" \
  "@/developer/src/kmc-generate" \
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
  "--build-path=BUILD_PATH   build directory for bundle"

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

function do_test() {
  typescript_run_eslint_mocha_tests 45
  builder_launch /developer/src/kmc/test/command-line-tests.sh test
}

#-------------------------------------------------------------------------------------------------------------------

function do_api() {
  rm -rf ./build/messages
  mkdir -p ./build/messages
  node . message --format markdown --out-path build/messages
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

  if builder_is_ci_build && builder_is_ci_build_level_release; then
    # Only inject sourcemaps for release builds
    sentry-cli sourcemaps inject \
      --org keyman \
      --project keyman-developer \
      --release "$KEYMAN_VERSION_GIT_TAG"  \
      build/dist/ "${SOURCEMAP_PATHS[@]}"
  fi

  # Manually copy over kmcmplib module
  cp ../kmc-kmn/build/src/import/kmcmplib/wasm-host.wasm build/dist/

  # Manually copy over templates
  cp -R ../kmc-generate/build/src/template/ build/dist/

  cp -R build/dist/* "$BUILD_PATH"

  builder_finish_action success bundle
}

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean      rm -rf ./build/ ./tsconfig.tsbuildinfo
builder_run_action configure  node_select_version_and_npm_ci
builder_run_action build      do_build
builder_run_action test       do_test
builder_run_action api        do_api
builder_run_action bundle     do_bundle
