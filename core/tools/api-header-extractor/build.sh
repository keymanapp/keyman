#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

CORE_ROOT="$KEYMAN_ROOT/core"
CORE_H_FILE="$CORE_ROOT/include/keyman/keyman_core_api.h"
CORE_H_DOC="index.md"
OUTPATH="$CORE_ROOT/build/doc"

################################ Main script ################################

builder_describe "Extract documentation from header files" clean configure build run
builder_parse "$@"

builder_run_action clean   rm -rf "${OUTPATH}"
builder_run_action build   tsc --build

if builder_start_action run; then
  rm -rf "$OUTPATH"
  mkdir -p "$OUTPATH"
  node --enable-source-maps ./build/src/index.js "${CORE_H_FILE}" "${OUTPATH}"
  echo "Wrote documentation to ${OUTPATH}/${CORE_H_DOC}"

  # if [ ! -z "${HELP_KEYMAN_COM+x}" ]; then
  #   # TODO move this to help-keyman-com.sh
  #   rm -rf "${HELP_KEYMAN_COM}developer/core/desktop/17.0/c-api/"
  #   mkdir -p "${HELP_KEYMAN_COM}developer/core/desktop/17.0/c-api/"
  #   cp "${OUTPATH}/"* "${HELP_KEYMAN_COM}developer/core/desktop/17.0/c-api/"
  # fi

  builder_finish_action success run
fi

