#!/usr/bin/env bash

set -eu

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

if builder_start_action clean; then
  make clean
  builder_finish_action success clean
fi

if builder_start_action configure; then
  builder_finish_action success configure
fi

if builder_start_action build; then
  make
  builder_finish_action success build
fi

if builder_start_action test; then
  ./run-tests.sh
  builder_finish_action success test
fi

if builder_start_action install; then
  make install
  builder_finish_action success install
fi

if builder_start_action uninstall; then
  make uninstall
  builder_finish_action success uninstall
fi
