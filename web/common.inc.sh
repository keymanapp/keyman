#!/usr/bin/env bash
#

BUNDLE_CMD="node $KEYMAN_ROOT/common/tools/es-bundling/build/common-bundle.mjs"

# Compiles all build products corresponding to the specified target.
# This should be called from the working directory of a child project's
# build script.
#
# ### Parameters
#
# * 1: `product`    the product's source path under src/
# * 2: `src_dir`    the source directory. Optional. Default: ${KEYMAN_ROOT}/web/src
# * 3: `build_dir`  the build directory. Optional. Default: ${KEYMAN_ROOT}/web/build
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
  local SRC_DIR=${2:-"${KEYMAN_ROOT}/web/src"}
  local BUILD_DIR=${3:-"${KEYMAN_ROOT}/web/build"}

  tsc -b "${SRC_DIR}/$COMPILE_TARGET"

  # So... tsc does declaration-bundling on its own pretty well, at least for local development.
  tsc --emitDeclarationOnly --outFile "${BUILD_DIR}/$COMPILE_TARGET/lib/index.d.ts" -p "${SRC_DIR}/$COMPILE_TARGET"
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
  TEST_BASE="${KEYMAN_ROOT}/web/src/test/auto/headless/"
  TEST_EXTENSIONS=${2:-}
  if [ ! -z "${2:-}" ]; then
    TEST_BASE="${KEYMAN_ROOT}/web/build/test/headless/"

    # Ensure the compiled tests are available.
    tsc --project "${KEYMAN_ROOT}/web/src/test/auto/tsconfig.json"
  fi

  TEST_OPTS=
  TEST_CD_REQD=false
  if builder_has_option --ci; then
    TEST_OPTS="--reporter mocha-teamcity-reporter"
  fi
  if [[ -n "$TEST_EXTENSIONS" ]]; then
    TEST_OPTS="$TEST_OPTS --extension $TEST_EXTENSIONS"
    TEST_CD_REQD=true
  fi

  if [ $TEST_CD_REQD ]; then
    # The mocha config needed to live-compile TS-based tests only applies
    # if the command is started within the appropriate subfolder.
    pushd "${TEST_BASE}" > /dev/null
    TEST_BASE=
  fi

  if [[ -e .c8rc.json ]]; then
    c8 mocha --recursive "${TEST_BASE}${TEST_FOLDER}" $TEST_OPTS
  else
    mocha --recursive "${TEST_BASE}${TEST_FOLDER}" $TEST_OPTS
  fi

  if [ $TEST_CD_REQD ]; then
    popd > /dev/null
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
  # tests.js - ensure any plain-js files that exist as test resources, but not test defs,
  # aren't treated by Mocha as tests.
  test-headless "$1" "tests.js"
}
