#!/usr/bin/env bash
#
# Builds /core/include/ldml/keyman_core_ldml.h from /core/include/ldml/keyman_core_ldml.ts
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"

CORE_LDML_H_FILE="../../include/ldml/keyman_core_ldml.h"
CORE_LDML_TS_FILE="../../include/ldml/keyman_core_ldml.ts"

################################ Main script ################################

builder_describe "Build and run the constant builder for LDML" clean configure build run test

builder_parse "$@"

builder_describe_internal_dependency \
  run:project    build:project \
  test:project   run:project

builder_describe_outputs \
  configure "/node_modules" \
  build     "/core/include/ldml/ldml-const-builder/ldml-const-builder.js" \
  run       "/core/include/ldml/ldml-const-builder/run.txt"

function do_run() {
  node --enable-source-maps ../../include/ldml/ldml-const-builder/ldml-const-builder.js > ${CORE_LDML_H_FILE}
  touch ../../include/ldml/ldml-const-builder/run.txt
  echo "Updated ${CORE_LDML_H_FILE}"
}

function do_test() {
  if ! git diff --exit-code "${CORE_LDML_H_FILE}"; then
    builder_echo error "File ${CORE_LDML_TS_FILE} has changed, but /core/tools/ldml-const-builder has not been run."
    builder_echo error "Run ${COLOR_BRIGHT_WHITE}$KEYMAN_ROOT/core/tools/ldml-const-build/build.sh run${COLOR_RESET} and commit changes"
    return 1
  fi
}

# clean: Not removing ${CORE_LDML_H_FILE} as it is checked in
builder_run_action clean      rm -rf ../../include/ldml/build/ ../../include/ldml/ldml-const-builder/ ../../include/ldml/coverage/
builder_run_action configure  node_select_version_and_npm_ci
builder_run_action build      tsc -b ../../include/ldml/tsconfig.build.json
builder_run_action run        do_run
builder_run_action test       do_test

