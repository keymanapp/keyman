#!/bin/bash
#
# Compiles the developer tools, including the language model compilers.
#

# Exit on command failure and when using unset variables:
set -eu

# Include some helper functions from resources
. ../../resources/shellHelperFunctions.sh
EX_USAGE=64


# Build the main script.
build () {
  npm run build || fail "Could not build top-level JavaScript file."
}

display_usage ( ) {
  echo "Usage: $0 [-test]"
  echo "       $0 -help"
  echo
  echo "  -help               displays this screen and exits"
  echo "  -test               runs unit tests after building"
}

################################ Main script ################################

run_tests=0

# Process command-line arguments
while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    -help|-h)
      display_usage
      exit
      ;;
    -test)
      run_tests=1
      ;;
    *)
      echo "$0: invalid option: $key"
      display_usage
      exit $EX_USAGE
  esac
  shift # past the processed argument
done

# Check if Node.JS/npm is installed.
type npm >/dev/null ||\
    fail "Build environment setup error detected!  Please ensure Node.js is installed!"

build || fail "Compilation failed."
echo "Typescript compilation successful."

if (( run_tests )); then
  npm test || fail "Tests failed"
fi
