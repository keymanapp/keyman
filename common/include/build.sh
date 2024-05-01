#!/usr/bin/env bash
#
# Creates keymanversion_build.h
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Build keymanversion_build.h" configure build clean

builder_describe_outputs \
  configure     /common/include/.keymanversion_build.configured \
  build         /common/include/keymanversion_build.h

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

if builder_start_action clean; then
  rm -f keymanversion_build.h .keymanversion_build.configured
  builder_finish_action success clean
fi

if builder_start_action configure; then
  # no-op, keeping 'configure' for deps
  touch .keymanversion_build.configured
  builder_finish_action success configure
fi

if builder_start_action build; then
  replaceVersionStrings_Mkver keymanversion_build.in keymanversion_build.h
  builder_finish_action success build
fi
