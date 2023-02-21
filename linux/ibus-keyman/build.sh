#!/usr/bin/env bash

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

################################ Main script ################################

builder_describe \
  "Build ibus-keyman." \
  ":engine" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "install                   install artifacts" \
  "uninstall                 uninstall artifacts" \
  "--debug,-d                Debug build" \
  "--output=OUTPUT_PATH,-o   Output path (default: ../build/)"
# We can't yet depend on core until it moved to the new build.sh syntax
# (currently it doesn't know some parameters that we're passing)
#  "@/core configure build"

builder_parse "$@"

if builder_has_option --debug; then
  MESON_TARGET=debug
  export CPPFLAGS=-DG_MESSAGES_DEBUG
  export CFLAGS="-O0"
  export CXXFLAGS="-O0"
else
  MESON_TARGET=release
fi
if builder_has_option --output; then
  OUTPUT_PATH=$(readlink -f "$OUTPUT_PATH")
else
  OUTPUT_PATH="$THIS_SCRIPT_PATH/../build"
fi
MESON_PATH="$OUTPUT_PATH/$(uname -m)/$MESON_TARGET"

builder_describe_outputs \
  configure "${MESON_PATH}/build.ninja" \
  build "${MESON_PATH}/src/ibus-engine-keyman" \
  build "${MESON_PATH}/tests/ibus-keyman-tests"

if builder_start_action clean; then
  rm -rf "${OUTPUT_PATH:?}/"
  builder_finish_action success clean
fi

if builder_start_action configure; then
  cd "$THIS_SCRIPT_PATH"
  # shellcheck disable=SC2086
  meson setup "$MESON_PATH" --werror --buildtype $MESON_TARGET "${builder_extra_params[@]}"
  builder_finish_action success configure
fi

if builder_start_action build; then
  cd "$MESON_PATH"
  ninja
  builder_finish_action success build
fi

if builder_start_action test; then
  cd "$MESON_PATH"
  meson test --print-errorlogs $builder_verbose
  builder_finish_action success test
fi

if builder_start_action install; then
  cd "$MESON_PATH"
  ninja install
  builder_finish_action success install
fi

if builder_start_action uninstall; then
  cd "$MESON_PATH"
  ninja uninstall
  builder_finish_action success uninstall
fi
