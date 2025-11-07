#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"

builder_describe "Build Keyman langtags.js common module" \
  "clean" \
  "configure" \
  "build" \
  "test"

builder_describe_outputs \
  configure   /common/web/langtags/src/imports/langtags.js \
  build       /common/web/langtags/build/src/main.js

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

function compile_langtags() {
  mkdir -p src/imports
  echo 'export default ' > src/imports/langtags.js
  cat "$KEYMAN_ROOT/resources/standards-data/langtags/langtags.json" >> src/imports/langtags.js
}

function do_configure() {
  node_select_version_and_npm_ci
  compile_langtags
}

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean      rm -rf ./build ./src/imports ./node_modules
builder_run_action configure  do_configure
builder_run_action build      tsc --build
builder_run_action test       echo 'no tests for langtags'
