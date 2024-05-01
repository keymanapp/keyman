#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

################################ Main script ################################

builder_describe \
  "Build Keyman for Linux." \
  ":config=keyman-config             keyman-config" \
  ":engine=ibus-keyman               ibus-keyman" \
  ":service=keyman-system-service    keyman-system-service" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "install                   install artifacts" \
  "uninstall                 uninstall artifacts" \
  "--no-integration+         don't run integration tests" \
  "--coverage+               capture test coverage" \
  "--report+                 create coverage report" \
  "--open                    open the coverage reports in the browser"

builder_parse "$@"

builder_run_child_actions clean configure build test install uninstall

test_action() {
  if builder_has_option --open; then
    builder_echo "Opening coverage reports in browser..."
    xdg-open "file://${THIS_SCRIPT_PATH}/CodeCoverageReports.html"
  fi
}

builder_run_action test test_action
