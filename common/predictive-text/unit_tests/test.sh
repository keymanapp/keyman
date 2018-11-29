#! /bin/bash

# We should work within the script's directory, not the one we were called in.
cd $(dirname "$BASH_SOURCE")

# Include useful testing resource functions
. ../../../resources/shellHelperFunctions.sh

# A simple utility script to facilitate unit-testing for the LM Layer.
# It's rigged to be callable by NPM to facilitate testing during development when in other folders.

display_usage ( ) {
  echo "test.sh [ -? | -h | -help]"
  echo "  -ci               to perform continuous-integration friendly tests and reporting"
  echo "  -headless         to disable the in-browser tests"
  echo "  -integrated       to disable the 'headless' test suite"
  echo "  -? | -h | -help   to display this help information"
  echo ""
  exit 0
}

init_dependencies ( ) {
  # Ensure all testing dependencies are in place.
  npm install
}

test-headless ( ) {
  if (( CI_REPORTING )); then
    FLAGS="${FLAGS} --reporter mocha-teamcity-reporter"
  fi

  npm run mocha -- --recursive ${FLAGS} ./unit_tests/headless/*.js
}

# Defaults
get_builder_OS  # return:  os_id="linux"|"mac"|"win" 

FLAGS=
CI_REPORTING=0
RUN_HEADLESS=1
RUN_BROWSERS=1
# Parse args
while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    -h|-help|-?)
      display_usage
      exit
      ;;
    -ci)
      CI_REPORTING=1
      ;;
    -headless)
      RUN_BROWSERS=0
      ;;
    -integrated)
      RUN_HEADLESS=0
      ;;
  esac
  shift # past argument
done

init_dependencies

BASE_PATH=`dirname $BASH_SOURCE`

# Run headless (browserless) tests.
if (( RUN_HEADLESS )); then
  test-headless || fail "DOMless tests failed!"
fi

# Run browser-based tests.
if (( RUN_BROWSERS )); then
  $BASE_PATH/in_browser/browser-test.sh $os_id
  CODE=$?
  exit $CODE
fi