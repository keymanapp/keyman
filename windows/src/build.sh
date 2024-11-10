#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

builder_describe \
  "Keyman for Windows" \
  \
  "@/resources/tools/check-markdown  test:help" \
  \
  clean \
  configure \
  build \
  test \
  "publish                 Prepare files for distribution, publish symbols, and build installer module" \
  "install                 Install built programs locally" \
  \
  ":engine                 Keyman Engine for Windows" \
  ":desktop                Keyman for Windows" \
  ":help                   Online documentation" \
  ":components=global/delphi   Delphi components" \
  ":support                Support tools" \
  ":test=test/unit-tests   Shared unit tests" \
  ":fv=../../oem/firstvoices/windows/src/inst           OEM FirstVoices for Windows app"

builder_parse "$@"

builder_run_child_actions  clean configure build test

function do_test_help() {
  check-markdown  "$KEYMAN_ROOT/windows/docs/help"
  check-markdown  "$KEYMAN_ROOT/windows/docs/engine"
}

builder_run_action         test:help      do_test_help
builder_run_child_actions  publish install
