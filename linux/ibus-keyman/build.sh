#!/usr/bin/env bash

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
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
  "report                    create coverage report" \
  "@/core:arch" \
  "--no-integration          don't run integration tests" \
  "--coverage                capture test coverage"

builder_parse "$@"

builder_describe_internal_dependency \
  report:engine test:engine

if builder_is_debug_build; then
  MESON_TARGET=debug
  export CPPFLAGS=-DG_MESSAGES_DEBUG
  export CFLAGS="-O0"
  export CXXFLAGS="-O0"
else
  MESON_TARGET=release
fi
MESON_PATH="../build/$(uname -m)/$MESON_TARGET"

cd "$THIS_SCRIPT_PATH"

builder_describe_outputs \
  configure "${MESON_PATH}/build.ninja" \
  build "${MESON_PATH}/src/ibus-engine-keyman"

if builder_has_option --coverage; then
  MESON_COVERAGE=-Db_coverage=true
else
  MESON_COVERAGE=
fi

if builder_start_action clean; then
  rm -rf "$THIS_SCRIPT_PATH/../build/"
  builder_finish_action success clean
fi

if builder_start_action configure; then
  cd "$THIS_SCRIPT_PATH"
  # shellcheck disable=SC2086
  meson setup "$MESON_PATH" --werror --buildtype $MESON_TARGET ${MESON_COVERAGE} "${builder_extra_params[@]}"
  builder_finish_action success configure
fi

if builder_start_action build; then
  cd "$THIS_SCRIPT_PATH/$MESON_PATH"
  ninja
  builder_finish_action success build
fi

if builder_start_action test; then
  cd "$THIS_SCRIPT_PATH/$MESON_PATH"
  if builder_has_option --no-integration; then
    meson test --print-errorlogs $builder_verbose setup-src-test keymanutil-tests print-kmpdetails-test print-kmp-test bcp47-util-tests teardown-src-test
  else
    meson test --print-errorlogs $builder_verbose
  fi
  builder_finish_action success test
fi

if builder_start_action install; then
  cd "$THIS_SCRIPT_PATH/$MESON_PATH"
  ninja install
  builder_finish_action success install
fi

if builder_start_action uninstall; then
  cd "$THIS_SCRIPT_PATH/$MESON_PATH"
  ninja uninstall
  builder_finish_action success uninstall
fi

if builder_start_action report; then
  cd "$THIS_SCRIPT_PATH/$MESON_PATH"
  # Note: requires lcov > 1.16 to properly work (see https://github.com/mesonbuild/meson/issues/6747)
  ninja coverage-html
  builder_finish_action success report
fi
