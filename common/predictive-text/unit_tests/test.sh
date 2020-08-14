#! /bin/bash

# We should work within the script's directory, not the one we were called in.
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
SCRIPT_ROOT="$(dirname "$THIS_SCRIPT")"

# A simple utility script to facilitate unit-testing for the LM Layer.
# It's rigged to be callable by NPM to facilitate testing during development when in other folders.

display_usage ( ) {
  echo "test.sh [ -? | -h | -help]"
  echo "  -CI               to perform continuous-integration friendly tests and reporting"
  echo "  -headless         to disable the in-browser tests"
  echo "  -integrated       to disable the 'headless' test suite"
  echo "  -? | -h | -help   to display this help information"
  echo ""
  exit 0
}

init_dependencies ( ) {
  # Ensure all testing dependencies are in place.
  verify_npm_setup
}

test-headless ( ) {
  _FLAGS=$FLAGS
  if (( CI_REPORTING )); then
    _FLAGS="$_FLAGS --reporter mocha-teamcity-reporter"
  fi

  npm run mocha -- --recursive $_FLAGS ./unit_tests/headless/*.js ./unit_tests/headless/**/*.js
}

test-browsers ( ) {
  _FLAGS=$FLAGS
  if (( CI_REPORTING )); then
    _FLAGS="$_FLAGS -CI -reporter teamcity"
  fi

  $SCRIPT_ROOT/in_browser/browser-test.sh $os_id $_FLAGS
}

# Defaults
get_builder_OS  # return:  os_id="linux"|"mac"|"win" 

FLAGS="--require ./unit_tests/helpers"
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
    -CI)
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

# Run headless (browserless) tests.
if (( RUN_HEADLESS )); then
  test-headless || fail "DOMless tests failed!"
fi

# Run browser-based tests.
if (( RUN_BROWSERS )); then
  test-browsers || fail "Browser-based tests failed!"
fi
