#!/usr/bin/env bash
#
# Builds /core/include/ldml/keyboardprocessor_ldml.h from /core/include/ldml/keyboardprocessor_ldml.ts
#

# Exit on command failure and when using unset variables:
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$(dirname "$THIS_SCRIPT")"

KBP_LDML_H_FILE="../../include/ldml/keyboardprocessor_ldml.h"

################################ Main script ################################

builder_describe "Build and run the constant builder for LDML" clean build run
builder_parse "$@"

# TODO: build if out-of-date if test is specified
# TODO: configure if npm has not been run, and build is specified


if builder_start_action clean; then
  rm -rf ../../include/ldml/build/
  # Not removing ${KBP_LDML_H_FILE} as it is checked in
  builder_finish_action success clean
fi

if builder_start_action build; then
  # Generate index.ts
  npx tsc -b ../../include/ldml/tsconfig.build.json

  builder_finish_action success build
fi

if builder_start_action run; then
  node --enable-source-maps ../../include/ldml/ldml-const-builder/ldml-const-builder.js > ${KBP_LDML_H_FILE}
  echo "Updated ${KBP_LDML_H_FILE}"

  builder_finish_action success run
fi

