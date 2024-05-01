#!/usr/bin/env bash
#
# Compile KeymanWeb's dev & test tool modules
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

################################ Main script ################################

builder_describe "Builds the Keyman Engine for Web's development & unit-testing tools" \
  "@/common/web/keyman-version" \
  "@/common/web/keyboard-processor" \
  "configure" \
  "clean" \
  "build" \
  "test" \
  "--ci         Does nothing for this script" \
  ":bulk_rendering=testing/bulk_rendering   Builds the bulk-rendering tool used to validate changes to OSK display code" \
  ":recorder=testing/recorder               Builds the KMW recorder tool used for development of unit-test resources" \
  ":sourcemap-root=building/sourcemap-root  Builds the sourcemap-cleaning tool used during minification of app/ builds"

builder_parse "$@"

builder_run_child_actions clean

# Some of the web/src/test section uses this script as a dependency.  We can skip
# the configure sections in such a case.
if ! builder_is_dep_build && ! builder_is_child_build; then
  builder_run_child_actions configure
fi

builder_run_child_actions build