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
. "$(dirname "$THIS_SCRIPT")/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# This script runs from its own folder
cd "$(dirname "$THIS_SCRIPT")"

WORKER_OUTPUT=build/obj
WORKER_OUTPUT_FILENAME=build/lib/worker-main.js
WORKER_WRAPPED_BUNDLE_TARGET_FILENAME=build/lib/worker-main.wrapped-for-bundle.js

################################ Main script ################################

builder_describe \
  "Compiles the Language Modeling Layer for common use in predictive text and autocorrective applications." \
  "@../keyman-version" \
  "@../../tools/sourcemap-path-remapper" \
  configure clean build test \
  "--ci      Runs unit tests with CI reporting" \
  "--minify  Minifies the resulting build product" \
  "--debug   Includes full sources in the worker's sourcemap"

builder_describe_outputs \
  configure     /node_modules \
  build         build/lib/worker-main.bundled.js

builder_parse "$@"

# TODO: build if out-of-date if test is specified
# TODO: configure if npm has not been run, and build is specified

if builder_start_action configure; then
  verify_npm_setup
  builder_finish_action success configure
fi

if builder_start_action clean; then
  npm run clean
  builder_finish_action success clean
fi

if builder_start_action build; then
  # Build worker with tsc first
  npm run build -- $builder_verbose || fail "Could not build worker."


  echo "Bundling worker modules"
  node build-bundler.js

  OPTIONS=
  if builder_has_option --debug; then
    OPTIONS="$OPTIONS --debug"
  fi
  if builder_has_option --minify; then
    OPTIONS="$OPTIONS --minify"
  fi

  echo "Preparing the polyfills + worker for script-embedding"
  node worker-wrapper-bundler.js $OPTIONS

  builder_finish_action success build
fi

if builder_start_action test; then
  MOCHA_FLAGS=

  if builder_has_option --ci; then
    MOCHA_FLAGS="$MOCHA_FLAGS --reporter mocha-teamcity-reporter"
  fi

  npm run mocha -- --recursive $MOCHA_FLAGS ./src/test/helpers.js ./src/test/cases/

  builder_finish_action success test
fi
