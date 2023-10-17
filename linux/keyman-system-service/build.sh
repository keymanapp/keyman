#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

################################ Main script ################################

builder_describe \
  "Build keyman-system-service." \
  ":service" \
  "clean" \
  "configure" \
  "build" \
  "test" \
  "install                   install artifacts" \
  "uninstall                 uninstall artifacts" \
  "--no-integration          don't run integration tests" \
  "--report                  create coverage report" \
  "--coverage                capture test coverage"

builder_parse "$@"

cd "$THIS_SCRIPT_PATH/"

if builder_is_debug_build; then
  MESON_TARGET=debug
  export CPPFLAGS=-DG_MESSAGES_DEBUG
  export CFLAGS="-O0"
  export CXXFLAGS="-O0"
else
  MESON_TARGET=release
fi
MESON_PATH="build/$(uname -m)/$MESON_TARGET"

builder_describe_outputs \
  configure "${MESON_PATH}/build.ninja" \
  build "${MESON_PATH}/src/keyman-system-service"

if builder_has_option --coverage; then
  MESON_COVERAGE=-Db_coverage=true
else
  MESON_COVERAGE=
fi

configure_action() {
  # shellcheck disable=SC2086,SC2154
  meson setup ${MESON_COVERAGE} --werror --buildtype $MESON_TARGET "${builder_extra_params[@]}" "$MESON_PATH"
}

test_action() {
  # shellcheck disable=SC2086,SC2154
  meson test --print-errorlogs $builder_verbose
  if builder_has_option --coverage; then
    # Note: requires lcov > 1.16 to properly work (see https://github.com/mesonbuild/meson/issues/6747)
    ninja coverage-html
  fi
}

builder_run_action clean       rm -rf "$THIS_SCRIPT_PATH/build/"
builder_run_action configure   configure_action

[ -d "$MESON_PATH" ] && cd "$MESON_PATH"

builder_run_action build       ninja
builder_run_action test        test_action
builder_run_action install     ninja install
builder_run_action uninstall   ninja uninstall
