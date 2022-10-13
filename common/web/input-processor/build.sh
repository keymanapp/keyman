#!/usr/bin/env bash
#
# Compile KeymanWeb's 'keyboard-processor' module, one of the components of Web's 'core' module.
#
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# This script runs from its own folder
cd "$(dirname "$THIS_SCRIPT")"

################################ Main script ################################

builder_check_color "$@"

builder_describe "Builds the standalone, headless form of Keyman Engine for Web's input-processor module" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  ":libraries  Targets all in-repo libraries that this module is dependent upon" \
  ":module     A headless, Node-oriented version of the module useful for unit tests" \
  ":tools      Related tools useful for development and testing of this module" \
  "--ci        Sets ${BUILDER_TERM_START}test${BUILDER_TERM_END} action to use CI-based test configurations & reporting"

builder_parse "$@"

### CONFIGURE ACTIONS

do_configure() {
  # Check if Node.JS/npm is installed.
  verify_npm_setup

  "$KEYMAN_ROOT/common/web/keyman-version/build.sh"
}

CONFIGURED=
if builder_start_action configure :libraries; then
  do_configure
  CONFIGURED=configure:libraries

  builder_finish_action success configure :libraries
fi


if builder_start_action configure :module; then
  if [ -n "$CONFIGURED" ]; then
    echo "Configuration already completed in ${BUILDER_TERM_START}${CONFIGURED}${BUILDER_TERM_END}; skipping."
  else
    do_configure
    CONFIGURED=configure:module
  fi
  builder_finish_action success configure :module
fi

if builder_start_action configure :tools; then
  if [ -n "$CONFIGURED" ]; then
    echo "Configuration already completed in ${BUILDER_TERM_START}${CONFIGURED}${BUILDER_TERM_END}; skipping."
  else
    do_configure
    CONFIGURED=configure:tools
  fi

  builder_finish_action success configure :tools
fi

### CLEAN ACTIONS

MODULE_OUTPUT=build

# A nice, extensible method for -clean operations.  Add to this as necessary.
do_clean() {
  if [ -d $MODULE_OUTPUT ]; then
    rm -rf "$MODULE_OUTPUT"
  fi
}

CLEANED=
if builder_start_action clean :libraries; then
  do_clean
  CLEANED=clean:libraries

  builder_finish_action success clean :libraries
fi

if builder_start_action clean :module; then
  if [ -n "$CLEANED" ]; then
    echo "${BUILDER_TERM_START}clean${BUILDER_TERM_END} already completed as ${BUILDER_TERM_START}${CLEANED}${BUILDER_TERM_END}; skipping."
  else
    do_clean
    CLEANED=clean:module
  fi
  builder_finish_action success clean :module
fi

if builder_start_action clean :tools; then
  if [ -n "$CLEANED" ]; then
    echo "${BUILDER_TERM_START}clean${BUILDER_TERM_END} already completed as ${BUILDER_TERM_START}${CLEANED}${BUILDER_TERM_END}; skipping."
  else
    do_clean
    CLEANED=clean:tools
  fi

  builder_finish_action success clean :tools
fi

### BUILD ACTIONS

if builder_start_action build :libraries; then
  # Ensure that the LMLayer compiles properly, readying the build product for comsumption by KMW.
  "$KEYMAN_ROOT/common/predictive-text/build.sh" build:headless
  # Also ensure that the keyboard-processor module builds appropriately.
  "$KEYMAN_ROOT/common/web/keyboard-processor/build.sh" build

  builder_finish_action success build :libraries
fi

if builder_start_action build :tools; then
  # Used by test:module
  pushd "$KEYMAN_ROOT/developer/src/kmlmc"
  ./build.sh -S
  popd

  builder_finish_action success build :tools
fi

if builder_start_action build :module; then
  npm run tsc -- -b src/tsconfig.json

  builder_finish_action success build :module
fi

# TEST ACTIONS

if builder_start_action test :tools; then
  builder_finish_action success test :tools
fi

if builder_start_action test :libraries; then
  CHAINING_FLAGS=
  if builder_has_option --ci; then
    CHAINING_FLAGS=--ci
  fi
  # First, run tests on the keyboard processor.
  pushd "$KEYMAN_ROOT/common/web/keyboard-processor"
  ./build.sh test $CHAINING_FLAGS
  popd

  # lm-layer tests are handled outside of this chain, at least by our existing CI processes.

  builder_finish_action success test :libraries
fi

if builder_start_action test :module; then
  FLAGS=
  if builder_has_option --ci; then
    FLAGS="--reporter mocha-teamcity-reporter"
  fi

  # Build the leaf-style, bundled version of input-processor for use in testing.
  npm run tsc -- -b src/tsconfig.bundled.json

  npm run mocha -- --recursive $FLAGS ./tests/cases/

  builder_finish_action success test :module
fi