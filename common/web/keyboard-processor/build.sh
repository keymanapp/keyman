#!/usr/bin/env bash
#
# Compile KeymanWeb's 'keyboard-processor' module, one of the components of Web's 'core' module.
#
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

################################ Main script ################################

builder_describe \
  "Compiles the web-oriented utility function module." \
  "@../recorder  test" \
  "@../keyman-version" \
  "@../utils" \
  configure \
  clean \
  build \
  test \
  "--ci    For use with action $(builder_term test) - emits CI-friendly test reports"

builder_describe_outputs \
  configure     /node_modules \
  build         build/lib/index.mjs

builder_parse "$@"

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

if builder_start_action clean; then
  npm run clean
  rm -rf ./build
  builder_finish_action success clean
fi

if builder_start_action build; then
  npm run tsc -- --build "$THIS_SCRIPT_PATH/tsconfig.json"
  node ./build-bundler.js

  # Declaration bundling.
  npm run tsc -- --emitDeclarationOnly --outFile ./build/lib/index.d.ts
  npm run tsc -- --emitDeclarationOnly --outFile ./build/lib/dom-keyboard-loader.d.ts -p src/keyboards/loaders/tsconfig.dom.json
  npm run tsc -- --emitDeclarationOnly --outFile ./build/lib/node-keyboard-loader.d.ts -p src/keyboards/loaders/tsconfig.node.json

  builder_finish_action success build
fi

if builder_start_action test; then
  echo_heading "Running Keyboard Processor test suite"

  MOCHA_FLAGS=
  KARMA_CONFIG="manual.conf.cjs"
  if builder_has_option --ci; then
    echo "Replacing user-friendly test reports with CI-friendly versions."
    MOCHA_FLAGS="$MOCHA_FLAGS --reporter mocha-teamcity-reporter"
    KARMA_CONFIG="CI.conf.cjs"
  fi

  npm run mocha -- --recursive $MOCHA_FLAGS ./tests/node/
  npm run karma -- start ./tests/dom/$KARMA_CONFIG

  builder_finish_action success test
fi