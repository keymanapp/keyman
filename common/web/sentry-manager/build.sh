#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "${KEYMAN_ROOT}/resources/build/utils.inc.sh"
. "${KEYMAN_ROOT}/resources/build/node.inc.sh"

builder_describe "Sentry error reporting module for embedded Keyman Engine for Web" \
  "@/common/tools/es-bundling" \
  "@/common/web/keyman-version" \
  clean configure build

builder_describe_outputs \
  configure          /node_modules \
  build              /common/web/sentry-manager/build/lib/index.js

builder_parse "$@"

# ---------------------------------------------------------------------------------

function do_build() {
  tsc --build "./tsconfig.json"

  node_es_bundle "${KEYMAN_ROOT}/common/web/sentry-manager/build/obj/src/index.js" \
    --out        "${KEYMAN_ROOT}/common/web/sentry-manager/build/lib/index.js" \
    --sourceRoot "@keymanapp/keyman/common/web/sentry-manager/build/lib/"
}

builder_run_action clean     rm -rf build/
builder_run_action configure node_select_version_and_npm_ci
builder_run_action build     do_build

