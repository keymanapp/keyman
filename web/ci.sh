#!/usr/bin/env bash
#
# Provides a platform-independent implementation of the build steps used for CI when
# building the Keyman Engine for Web.
#

# set -x
set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

WORKING_DIRECTORY=`pwd`

# This script runs from its own folder
cd "$THIS_SCRIPT_PATH"

# ################################ Main script ################################

builder_describe "Defines and implements the CI build steps for Keyman Engine for Web (KMW)." \
  "build" \
  "test             Runs all unit tests."  \
  "post-test        Runs post-test cleanup.  Should be run even if a prior step fails." \
  "validate-size    Runs the build-size comparison check" \
  "--debug          Runs this script in local-development mode; reports and tests will be locally logged"

builder_parse "$@"

####

if builder_start_action build; then
  # Build step:  since CI builds start (and should start) from scratch, run the following
  # three actions:
  # - configure:  retrieve all NPM dependencies
  # - clean:      make extra-sure that no prior build products exist.
  #               - also useful when validating this script on a local dev machine!
  # - build:      then do the ACTUAL build.
  ./build.sh configure clean build

  builder_finish_action success build
fi

if builder_start_action test; then
  # Testing step:  run ALL unit tests, including those of the submodules.

  # For only top-level build-product tests, specify the :engine target
  # For all others, specify only the :libraries target
  FLAGS=

  if ! builder_has_option --debug; then
    FLAGS=--ci
  fi

  ./test.sh $FLAGS

  builder_finish_action success test
fi

if builder_start_action post-test; then
  # Always make sure BrowserStack stuff is terminated after the test step.
  # Even if it fails and/or hangs.
  # No matter what.  Otherwise, it may leave a lock-file that breaks a later build!
  ../common/test/resources/test_kill_browserstack.sh

  builder_finish_action success post-test
fi

if builder_start_action validate-size; then
  # Performs the web build product size check as reported on Web test PRs.
  FLAGS=

  if ! builder_has_option --debug; then
    FLAGS=--write-status
  fi

  # Do not use --report when in --debug mode; it sets an error code that is VERY difficult to
  # catch with our set -e & `trap` setup.
  ./src/tools/building/check-build-size.sh $FLAGS

  builder_finish_action success validate-size
fi