#!/usr/bin/env bash
#
# Packages the @keymanapp/ldml-keyboard-constants package.
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"

builder_describe "Keyman ldml-keyboard-constants package" \
  "@/common/web/keyman-version" \
  "clean" \
  "configure" \
  "build" \
  "test"

builder_describe_outputs \
  configure     /node_modules \
  build         /core/include/ldml/build/keyman_core_ldml.js

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean      rm -rf ./build/
builder_run_action configure  node_select_version_and_npm_ci
builder_run_action build      tsc --build
# builder_run_action test       # no tests at this time
