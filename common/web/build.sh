#!/usr/bin/env bash
#
# Compiles and tests the common web modules
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

#
# TODO: future modules may include
#   :lm-message-types \
#   :sentry-manager \
#

builder_describe "Keyman common web modules" \
  :keyman-version \
  :types \
  :utils \
  clean \
  configure \
  build \
  test

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

builder_run_child_actions clean configure build test
