#!/usr/bin/env bash

set -eu

# Include useful testing resource functions
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# We should work within the script's directory, not the one we were called in.
cd "$THIS_SCRIPT_PATH"

# A simple utility script to facilitate unit-testing for the LM Layer.
# It's rigged to be callable by NPM to facilitate testing during development when in other folders.

display_usage ( ) {
  echo "test.sh [-skip-package-install|-S] [-CI] [ -? | -h | -help]"
  echo "  -CI                    to perform continuous-integration friendly tests and reporting formatted for TeamCity"
  echo "  -? | -h | -help        to display this help information"
  echo "  -skip-package-install  to bypass refreshing dependencies.  Useful when called by scripts that pre-fetch"
  echo "  (or -S)"
  echo ""
  exit 0
}

# Defaults
FLAGS=
CI_REPORTING=0
FETCH_DEPS=true
CHAINING_FLAGS=

# Parse args
while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    -h|-help|-\?)
      display_usage
      exit
      ;;
    -CI)
      CI_REPORTING=1
      CHAINING_FLAGS="$CHAINING_FLAGS --ci"
      ;;
    -skip-package-install|-S)
      FETCH_DEPS=false
      ;;
  esac
  shift # past argument
done

if (( CI_REPORTING )); then
  FLAGS="$FLAGS --reporter mocha-teamcity-reporter"
fi

if [ $FETCH_DEPS = true ]; then
  verify_npm_setup
fi

# Ensures that the lexical model compiler has been built locally.
echo_heading "Preparing Lexical Model Compiler for test use"
pushd "$KEYMAN_ROOT/developer/src/kmlmc/"
./build.sh
popd

test-headless ( ) {
  npm run mocha -- --recursive $FLAGS ./tests/cases/
}

if [ $FETCH_DEPS = true ]; then
  # Next, build the lm-worker in its proper, wrapped form
  pushd "$KEYMAN_ROOT/common/web/lm-worker"
  ./build.sh --report-scope
  popd
fi

# First, run tests on the keyboard processor.
pushd "$KEYMAN_ROOT/common/web/keyboard-processor"
./build.sh test $CHAINING_FLAGS --report-scope || fail "Tests failed by dependencies; aborting integration tests."
popd

# Build the leaf-style, bundled version of input-processor for use in testing.
npm run tsc -- -b src/tsconfig.bundled.json || fail "Failed to compile the core/web/input-processor module."

# Now we run our local tests.
echo_heading "Running Input Processor test suite"
test-headless || fail "Input Processor tests failed!"
