#!/usr/bin/env bash
#
# Compile KeymanWeb's dev & test tool modules
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"

################################ Main script ################################

builder_describe "Keyman Engine for Web unit-testing and manual test tools" \
  "configure" \
  "clean" \
  "build" \
  ":bulk_rendering         Bulk-rendering tool used to validate changes to OSK display code" \
  ":recorder               KMW recorder tool used for development of unit-test resources" \
  ":test-utils             Builds the test-utils module" \
  ":gesture-processor      Gesture processor test utilities"

builder_parse "$@"

builder_run_child_actions clean
builder_run_child_actions configure
builder_run_child_actions build
