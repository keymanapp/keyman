#!/usr/bin/env bash
#
# Compile KeymanWeb's dev & test tool modules
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"

################################ Main script ################################

builder_describe "KeymanWeb development & unit-testing tools" \
  "configure" \
  "clean" \
  "build" \
  ":building" \
  ":testing"

builder_parse "$@"

builder_run_child_actions clean
builder_run_child_actions configure
builder_run_child_actions build