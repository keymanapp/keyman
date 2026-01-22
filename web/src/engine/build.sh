#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# Imports common Web build-script definitions & functions

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"
. "$KEYMAN_ROOT/web/common.inc.sh"

# ################################ Main script ################################

builder_describe "Builds Keyman Engine for Web (KMW)" \
  "@/core:wasm                 build" \
  "@/common/tools/es-bundling  build" \
  "@/web/src/common/web-utils  build" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  ":predictive-text"

# Possible TODO?
# "upload-symbols   Uploads build product to Sentry for error report symbolification.  Only defined for $DOC_BUILD_EMBED_WEB" \

builder_describe_outputs \
  configure   /node_modules \
  build       /web/build/engine/lib/index.mjs

builder_parse "$@"

#### Build action definitions ####

copy_deps() {
  mkdir -p "${KEYMAN_ROOT}/web/src/engine/src/core-adapter/import/core/"
  # we don't need this file for release builds, but it's nice to have
  # for reference and auto-completion
  cp "${KEYMAN_ROOT}/core/build/wasm/${BUILDER_CONFIGURATION}/src/keymancore.d.ts" "${KEYMAN_ROOT}/web/src/engine/src/core-adapter/import/core/"

  mkdir -p "${KEYMAN_ROOT}/web/build/engine/obj/core-adapter/import/core/"
  cp "${KEYMAN_ROOT}/core/build/wasm/${BUILDER_CONFIGURATION}/src/keymancore.d.ts" "${KEYMAN_ROOT}/web/build/engine/obj/core-adapter/import/core/"
  cp "${KEYMAN_ROOT}/core/build/wasm/${BUILDER_CONFIGURATION}/src/"km-core{.js,.wasm} "${KEYMAN_ROOT}/web/build/engine/obj/core-adapter/import/core/"
  cp "${KEYMAN_ROOT}/core/build/wasm/${BUILDER_CONFIGURATION}/src/"km-core-node{.mjs,.wasm} "${KEYMAN_ROOT}/web/build/engine/obj/core-adapter/import/core/"

  cp "${KEYMAN_ROOT}/common/resources/fonts/keymanweb-osk.ttf" "${KEYMAN_ROOT}/web/src/resources/osk/"
}

do_build () {
  # eslint .

  copy_deps

  tsc -b .

  # So... tsc does declaration-bundling on its own pretty well, at least for local development.
  tsc --emitDeclarationOnly --outFile "${KEYMAN_ROOT}/web/build/engine/lib/index.d.ts" -p "."

  node_es_bundle "${KEYMAN_ROOT}/web/build/engine/obj/index.js" \
    --out        "${KEYMAN_ROOT}/web/build/engine/lib/index.mjs" \
    --format esm

  # TODO-web-core: should this be in test/?
  echo "Validating gesture model and set references"
  node src/osk/validate-gesture-specs.js
}

run_tests() {
  local OUTPUT_FILE FAILURE_COUNT
  # Remove stale coverage data
  rm -rf "${KEYMAN_ROOT}/web/build/coverage/raw/engine"

  # Unfortunately we get an error from the coverage report generation:
  # "TypeError [ERR_INVALID_URL_SCHEME]: The URL must be of scheme file"
  # The following lines ignore the exit code and instead check the number
  # of failed tests from the output.
  set +e
  OUTPUT_FILE=$(mktemp)
  test-headless engine "" 2>&1 | tee "${OUTPUT_FILE}"
  set -e

  FAILURE_COUNT=$(grep ' failing' "${OUTPUT_FILE}" | xargs | cut -f 1 -d' ')
  rm "${OUTPUT_FILE}"
  builder_echo "(The 'TypeError [ERR_INVALID_URL_SCHEME]: The URL must be of scheme file' is expected)"
  if ((FAILURE_COUNT > 0)); then
    builder_die "Headless engine tests failed (.js tests)"
  fi

  test-headless-typescript engine
}

builder_run_action clean rm -rf "${KEYMAN_ROOT}/web/build/engine"
builder_run_child_actions clean
builder_run_action configure node_select_version_and_npm_ci
builder_run_child_actions configure
builder_run_child_actions build
builder_run_action build do_build
builder_run_action test  run_tests
builder_run_child_actions test