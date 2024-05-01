#!/usr/bin/env bash
#
# Builds /core/include/ldml/keyman_core_ldml.h from /core/include/ldml/keyman_core_ldml.ts
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

CORE_LDML_H_FILE="../../include/ldml/keyman_core_ldml.h"

################################ Main script ################################

builder_describe "Build and run the constant builder for LDML" clean build run
builder_parse "$@"

# TODO: build if out-of-date if test is specified
# TODO: configure if npm has not been run, and build is specified


if builder_start_action clean; then
  rm -rf ../../include/ldml/build/
  # Not removing ${CORE_LDML_H_FILE} as it is checked in
  builder_finish_action success clean
fi

if builder_start_action build; then
  # Generate index.ts
  npx tsc -b ../../include/ldml/tsconfig.build.json

  builder_finish_action success build
fi

if builder_start_action run; then
  node --enable-source-maps ../../include/ldml/ldml-const-builder/ldml-const-builder.js > ${CORE_LDML_H_FILE}
  echo "Updated ${CORE_LDML_H_FILE}"

  builder_finish_action success run
fi

