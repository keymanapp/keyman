#! /bin/bash

# A simple utility script to facilitate unit-testing for the LM Layer.
# It's rigged to be callable by NPM to facilitate testing during development when in other folders.

display_usage ( ) {
  echo "test.sh [-h | -help]"
  echo "  -? | -h | -help   to display this help information"
  echo ""
  exit 0
}

# Designed to determine which set of browsers should be available for local testing,
# based upon the current system OS.
get_OS ( ) {
  # Default value, since it's the most general case/configuration to detect.
  os_id="linux"

  # Subject to change with future improvements.
  if [[ "${OSTYPE}" = "darwin"* ]]; then
    os_id="mac"
  elif [[ "${OSTYPE}" = "msys" ]]; then
    os_id="win"
  elif [[ "${OSTYPE}" = "cygwin" ]]; then
    os_id="win"
  fi
}

init_dependencies ( ) {
  # Ensure all testing dependencies are in place.
  npm install
}

# Defaults
get_OS
get_browser_set_for_OS

FLAGS=

# Parse args
while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    -h)
      display_usage
      ;;
    -help)
      display_usage
      ;;
    -?)
      display_usage
      ;;
  esac
  shift # past argument
done

init_dependencies

# Run headless (browserless) tests.
npm run mocha --recursive ./unit_tests/headless/*.js

CODE=$?

exit $CODE