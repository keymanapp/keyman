#!/usr/bin/env bash
#
# Compiles the Language Modeling Layer for common use in predictive text and autocorrective applications.
# Designed for optimal compatibility with the Keyman Suite.
#

# Exit on command failure and when using unset variables:
set -eu

# Include some helper functions from resources

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$(dirname "$THIS_SCRIPT")"

# Exit status on invalid usage.
EX_USAGE=64

LMLAYER_OUTPUT=build

# Builds the top-level JavaScript file for use in browsers (the second stage of compilation)
build-browser () {
  npm run tsc -- -b ./browser.tsconfig.json || fail "Could not build top-level browser-targeted JavaScript file."
}

# Builds the top-level JavaScript file for use on Node (the second stage of compilation)
build-headless () {
  npm run tsc -- -b ./tsconfig.json || fail "Could not build top-level node-targeted JavaScript file."
}

# A nice, extensible method for -clean operations.  Add to this as necessary.
clean ( ) {
  if [ -d $LMLAYER_OUTPUT ]; then
    rm -rf "$LMLAYER_OUTPUT" || fail "Failed to erase the prior build."
  fi
}

display_usage ( ) {
  echo "Usage: $0 [-clean] [-skip-package-install | -S] [-test | -tdd]"
  echo "       $0 -help"
  echo
  echo "  -clean                 to erase pre-existing build products before a re-build"
  echo "  -help                  displays this screen and exits"
  echo "  -skip-package-install  (or -S) skips dependency updates"
  echo "  -tdd                   skips dependency updates, builds, then runs unit tests only"
  echo "  -test                  runs unit and integration tests after building"
}

################################ Main script ################################

run_tests=0
fetch_deps=true
unit_tests_only=0

# Process command-line arguments
while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    -clean)
      clean
      ;;
    -help|-h)
      display_usage
      exit
      ;;
    -skip-package-install|-S)
      fetch_deps=false
      ;;
    -test)
      run_tests=1
      ;;
    -tdd)
      run_tests=1
      fetch_deps=false
      unit_tests_only=1
      ;;
    *)
      echo "$0: invalid option: $key"
      display_usage
      exit $EX_USAGE
  esac
  shift # past the processed argument
done

# Check if Node.JS/npm is installed.
verify_npm_setup $fetch_deps

if $fetch_deps; then
  # We need to build keyman-version and lm-worker with a script for now
  "$KEYMAN_ROOT/common/web/keyman-version/build.sh" || fail "Could not build keyman-version"
  "$KEYMAN_ROOT/common/web/lm-worker/build.sh" || fail "Could not build lm-worker"
fi

build-browser || fail "Browser-oriented compilation failed."
build-headless || fail "Headless compilation failed."
echo "Typescript compilation successful."

if (( run_tests )); then
  if (( unit_tests_only )); then
    npm run test -- -headless || fail "Unit tests failed"
  else
    npm test || fail "Tests failed"
  fi
fi
