#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

################################ Main script ################################

builder_describe "Extract documentation from header files" clean configure build
builder_parse "$@"

if [[ -v KEYMAN_PKG_BUILD ]]; then
  # KEYMAN_PKG_BUILD is defined in linux/debian/rules when running
  # a packaging build
  echo "Disabled for Ubuntu packaging as node environment is not available"
  exit 0
fi

#----------------------------------------------------------------------------

. "$KEYMAN_ROOT/resources/build/node.inc.sh"
. "$KEYMAN_ROOT/resources/build/typescript.inc.sh"

CORE_ROOT="$KEYMAN_ROOT/core"
CORE_H_FILES=(\
  "$CORE_ROOT/include/keyman/keyman_core_api.h" \
  "$CORE_ROOT/include/keyman/keyman_core_api_actions.h" \
  "$CORE_ROOT/include/keyman/keyman_core_api_context.h" \
  "$CORE_ROOT/include/keyman/keyman_core_api_debug.h" \
  "$CORE_ROOT/include/keyman/keyman_core_api_vkeys.h" \
)
OUTPATH="$CORE_ROOT/docs/api"

do_build_and_run() {
  tsc --build
  rm -rf "$OUTPATH"
  mkdir -p "$OUTPATH"
  node --enable-source-maps ./build/src/index.js "${OUTPATH}" "${CORE_H_FILES[@]}"
  builder_echo "API documentation saved to ${OUTPATH}/"
}

builder_run_action clean      rm -rf "${OUTPATH}"
builder_run_action configure  node_select_version_and_npm_ci
builder_run_action build      do_build_and_run
