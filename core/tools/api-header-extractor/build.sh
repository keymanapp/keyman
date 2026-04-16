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
CORE_H_FILE="$CORE_ROOT/include/keyman/keyman_core_api.h"
CORE_H_DOC="index.md"
OUTPATH="$CORE_ROOT/docs/api"

do_build_and_run() {
  tsc --build
  rm -rf "$OUTPATH"
  mkdir -p "$OUTPATH"
  node --enable-source-maps ./build/src/index.js "${CORE_H_FILE}" "${OUTPATH}"
  echo "Wrote documentation to ${OUTPATH}/${CORE_H_DOC}"
}

builder_run_action clean      rm -rf "${OUTPATH}"
builder_run_action configure  node_select_version_and_npm_ci
builder_run_action build      do_build_and_run
