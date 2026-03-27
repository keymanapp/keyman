#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

################################ Main script ################################

builder_describe "Keyman Engine for Web development tools" \
  "configure" \
  "clean" \
  "build" \
  ":sourcemap-root    Sourcemap-cleaning tool used during minification of app/ builds"

builder_parse "$@"

builder_run_child_actions clean
builder_run_child_actions configure
builder_run_child_actions build