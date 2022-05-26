#!/bin/bash

# We should work within the script's directory, not the one we were called in.
cd $(dirname "$BASH_SOURCE")

WORKING_DIRECTORY=`pwd`

# Include useful testing resource functions
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../../resources/build/build-utils.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
## END STANDARD BUILD SCRIPT INCLUDE

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
      ;;
    -skip-package-install|-S)
      FETCH_DEPS=false
      ;;
  esac
  shift # past argument
done

if [ $FETCH_DEPS = true ]; then
  verify_npm_setup
fi

# Ensures that the lexical model compiler has been built locally.
#echo_heading "Preparing Lexical Model Compiler for test use"
#pushd $WORKING_DIRECTORY/node_modules/@keymanapp/lexical-model-compiler
#npm run build
#popd

test-headless ( ) {
  if (( CI_REPORTING )); then
    FLAGS="$FLAGS --reporter mocha-teamcity-reporter"
  fi

  # Poor Man's Modules until we support ES6 throughout
  PREPEND=./tests/cases/prepend.js
  (cat ../../../../resources/web-environment/build/index.js; echo) > $PREPEND
  (cat ../utils/build/index.js; echo) >> $PREPEND
  (cat ../keyboard-processor/build/index.js; echo) >> $PREPEND
  (cat ../input-processor/build/index.js; echo) >> $PREPEND
  (cat tests/cases/inputProcessor.js; echo) >> $PREPEND

  # TODO: We will re-enable languageProcessor tests when we have sorted out
  #       paths and modules for lmc
  #(cat tests/cases/languageProcessor.js; echo) >> $PREPEND

  npm run mocha -- --recursive $FLAGS ./tests/cases/prepend.js

  rm $PREPEND
}

if [ $FETCH_DEPS = true ]; then
  # First, run tests on the keyboard processor.
  pushd "$KEYMAN_ROOT/common/core/web/keyboard-processor"
  ./test.sh -skip-package-install || fail "Tests failed by dependencies; aborting integration tests."
  popd
fi

# Now we run our local tests.
echo_heading "Running Input Processor test suite"
test-headless || fail "Input Processor tests failed!"
