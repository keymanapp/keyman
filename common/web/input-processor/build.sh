#!/usr/bin/env bash
#
# Compile KeymanWeb's 'keyboard-processor' module, one of the components of Web's 'core' module.
#
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# This script runs from its own folder
cd "$(dirname "$THIS_SCRIPT")"

################################ Main script ################################

# TODO: for predictive-text, we only need :headless, perhaps we should be splitting modules?
# TODO: remove :tools once kmlmc is a dependency for test:module

builder_describe "Builds the standalone, headless form of Keyman Engine for Web's input-processor module" \
  "@/common/web/keyman-version" \
  "@/common/web/keyboard-processor" \
  "@/common/predictive-text" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  ":module     A headless, Node-oriented version of the module useful for unit tests" \
  ":tools      Related tools useful for development and testing of this module" \
  "--ci        Sets $(builder_term test) action to use CI-based test configurations & reporting"

builder_describe_outputs \
  configure          /node_modules \
  configure:module   /node_modules \
  configure:tools    /node_modules \
  build:module       /common/web/input-processor/build/index.js \
  build:tools        /developer/src/kmc/build/src/kmlmc.js    # TODO: remove this once kmlmc is a dependency

builder_parse "$@"

### CONFIGURE ACTIONS

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

### CLEAN ACTIONS

if builder_start_action clean; then
  rm -rf build/
  builder_finish_action success clean
fi

### BUILD ACTIONS

if builder_start_action build:tools; then
  # Used by test:module
  # TODO: convert to a dependency once we have updated kmlmc to use builder script
  pushd "$KEYMAN_ROOT/developer/src/kmc-model"
  ./build.sh
  popd
  pushd "$KEYMAN_ROOT/developer/src/kmc"
  ./build.sh
  popd

  builder_finish_action success build:tools
fi

if builder_start_action build:module; then
  tsc -b src/tsconfig.json
  builder_finish_action success build:module
fi

# TEST ACTIONS

if builder_start_action test:module; then
  FLAGS=
  if builder_has_option --ci; then
    FLAGS="--reporter mocha-teamcity-reporter"
  fi

  # Build the leaf-style, bundled version of input-processor for use in testing.
  tsc -b src/tsconfig.bundled.json

  mocha --recursive $FLAGS ./tests/cases/

  builder_finish_action success test:module
fi