#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

builder_describe "Build Keyman kmc keyboard-info Compiler module" \
  "@/common/web/types" \
  "@/developer/src/common/web/utils" \
  "@/developer/src/kmc-package" \
  "clean" \
  "configure" \
  "build" \
  "api                       analyze API and prepare API documentation" \
  "test" \
  "publish                   publish to npm" \
  "--npm-publish+            For publish, do a npm publish, not npm pack (only for CI)" \
  "--dry-run,-n              don't actually publish, just dry run"

builder_describe_outputs \
  configure     /developer/src/kmc-keyboard-info/src/imports/langtags.js \
  build         /developer/src/kmc-keyboard-info/build/src/index.js \
  api           /developer/build/api/kmc-keyboard-info.api.json

builder_parse "$@"

function do_configure() {
  verify_npm_setup
  mkdir -p src/imports
  echo 'export default ' > src/imports/langtags.js
  cat "$KEYMAN_ROOT/resources/standards-data/langtags/langtags.json" >> src/imports/langtags.js
}

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean       rm -rf ./build/ ./tsconfig.tsbuildinfo
builder_run_action configure   do_configure
builder_run_action build       tsc --build
builder_run_action api         api-extractor run --local --verbose
builder_run_action test        builder_do_typescript_tests

#-------------------------------------------------------------------------------------------------------------------

. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"

builder_run_action publish      builder_publish_npm
