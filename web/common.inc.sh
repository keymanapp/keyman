#!/usr/bin/env bash
#

# Compiles all build products corresponding to the specified target.
# This should be called from the working directory of a child project's
# build script.
#
# ### Parameters
#
# * 1: `product`    the product's source path under src/
#
# ### Example
#
# ```bash
#   compile engine/main
# ```
function compile() {
  if [ $# -lt 1 ]; then
    builder_die "Scripting error: insufficient argument count!"
  fi

  local COMPILE_TARGET="$1"
  local BUNDLE_FLAG="${2:-}"

  tsc -b "${KEYMAN_ROOT}/web/src/$COMPILE_TARGET"

  if [ -f "./build-bundler.js" ]; then
    node "./build-bundler.js" "$BUNDLE_FLAG"

    # So... tsc does declaration-bundling on its own pretty well, at least for local development.
    tsc --emitDeclarationOnly --outFile "${KEYMAN_ROOT}/web/build/$COMPILE_TARGET/lib/index.d.ts" -p "${KEYMAN_ROOT}/web/src/$COMPILE_TARGET"
  fi
}

function _copy_dir_if_exists() {
  local SRC=$1
  local DST=$2

  if [ -d "$SRC" ]; then
    cp -rf "$SRC/." "$DST"
  fi
}

# Copies top-level build artifacts into common 'debug' and 'release' config folders
# for use in publishing and does a normalization pass on the sourcemaps.
function prepare() {
  local CHILD_BUILD_ROOT="$KEYMAN_ROOT/web/build/app"
  local PUBLISH_BUILD_ROOT="$KEYMAN_ROOT/web/build/publish"

  mkdir -p "$PUBLISH_BUILD_ROOT/debug"
  mkdir -p "$PUBLISH_BUILD_ROOT/release"

  _copy_dir_if_exists "$CHILD_BUILD_ROOT/browser/debug"    "$PUBLISH_BUILD_ROOT/debug"
  _copy_dir_if_exists "$CHILD_BUILD_ROOT/browser/release"  "$PUBLISH_BUILD_ROOT/release"

  _copy_dir_if_exists "$CHILD_BUILD_ROOT/resources"  "$PUBLISH_BUILD_ROOT/debug"
  _copy_dir_if_exists "$CHILD_BUILD_ROOT/resources"  "$PUBLISH_BUILD_ROOT/release"

  _copy_dir_if_exists "$CHILD_BUILD_ROOT/ui/debug"    "$PUBLISH_BUILD_ROOT/debug"
  _copy_dir_if_exists "$CHILD_BUILD_ROOT/ui/release"  "$PUBLISH_BUILD_ROOT/release"

  # Normalize paths in the sourcemaps
  for sourcemap in "$PUBLISH_BUILD_ROOT/debug/"*.map; do
    node "$KEYMAN_ROOT/web/build/tools/building/sourcemap-root/index.js" null "$sourcemap" --clean
  done

  for sourcemap in "$PUBLISH_BUILD_ROOT/release/"*.map; do
    node "$KEYMAN_ROOT/web/build/tools/building/sourcemap-root/index.js" null "$sourcemap" --clean
  done
}

# Runs all headless tests corresponding to the specified target.
# This should be called from the working directory of a child project's
# build script.
#
# ### Parameters
#
# * 1: `product`    the folder under src/test/auto/headless containing the
#                   child project's tests
# * 2: `base_dir`   the base directory containing the `product` folder.
#                   Optional. Default: ${KEYMAN_ROOT}/web/src/test/auto/headless/
#
# ### Example
#
# ```bash
#   # from engine/osk
#   test-headless engine/osk
# ```
function test-headless() {
  TEST_FOLDER=$1
  TEST_BASE=${2:-${KEYMAN_ROOT}/web/src/test/auto/headless/}

  TEST_OPTS=
  if builder_has_option --ci; then
    TEST_OPTS="--reporter mocha-teamcity-reporter"
  fi

  if [[ -e .c8rc.json ]]; then
    c8 mocha --recursive "${TEST_BASE}/${TEST_FOLDER}" $TEST_OPTS
  else
    mocha --recursive "${TEST_BASE}/${TEST_FOLDER}" $TEST_OPTS
  fi
}

# Runs all headless tests (written in typescript) corresponding to the
# specified target.
# This should be called from the working directory of a child project's
# build script.
#
# ### Parameters
#
# * 1: `product`    the folder under src/test/auto/headless containing the
#                   child project's tests
#
# ### Example
#
# ```bash
#   # from engine/osk
#   test-headless-typescript engine/osk
# ```
function test-headless-typescript() {
  test-headless "$1" "${KEYMAN_ROOT}/web/build/test/headless"
}
