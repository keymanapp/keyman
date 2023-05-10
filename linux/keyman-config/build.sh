#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
# shellcheck source=resources/build/build-utils.sh
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

################################ Main script ################################

builder_describe \
  "Build keyman-config." \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "install                   install artifacts" \
  "uninstall                 uninstall artifacts" \

builder_parse "$@"

cd "$THIS_SCRIPT_PATH"

builder_describe_outputs \
  build "keyman_config/standards/lang_tags_map.py"

builder_run_action clean      make clean
builder_run_action configure  # nothing to do
builder_run_action build      make
builder_run_action test       ./run-tests.sh
builder_run_action install    make install
builder_run_action uninstall  make uninstall
