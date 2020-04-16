#! /bin/bash

# We should work within the script's directory, not the one we were called in.
cd $(dirname "$BASH_SOURCE")

# Include useful testing resource functions
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
KEYMAN_ROOT="$(dirname "$THIS_SCRIPT")/../../../.."
. "$KEYMAN_ROOT/resources/build/build-utils.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# A simple utility script to facilitate unit-testing for the LM Layer.
# It's rigged to be callable by NPM to facilitate testing during development when in other folders.

display_usage ( ) {
  echo "test.sh [ -? | -h | -help]"
  echo "  -CI               to perform continuous-integration friendly tests and reporting"
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

  npm run mocha -- --recursive $_FLAGS ./tests/cases/*.js
}

# Defaults
get_builder_OS  # return:  os_id="linux"|"mac"|"win" 

FLAGS=
CI_REPORTING=0
RUN_HEADLESS=1

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
  esac
  shift # past argument
done

init_dependencies

# Run headless (browserless) tests.
if (( RUN_HEADLESS )); then
  test-headless || fail "Keyboard Processor tests failed!"
fi

