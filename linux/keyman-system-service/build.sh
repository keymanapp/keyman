#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder.inc.sh"
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

if builder_is_debug_build; then
  MESON_TARGET=debug
  export CPPFLAGS=-DG_MESSAGES_DEBUG
  export CFLAGS="-O0"
  export CXXFLAGS="-O0"
else
  MESON_TARGET=release
fi
MESON_PATH="build/$(uname -m)/$MESON_TARGET"

clean_action() {
  rm -rf "$THIS_SCRIPT_PATH/build/"
}

check_missing_coverage_configuration() {
  if builder_has_option --coverage && [ -d "$MESON_PATH" ]; then
    # It's possible that we got configured because we're a dependency of ibus-keyman
    # in which case the `--coverage` option wasn't passed along.
    cd "$MESON_PATH"
    if ! ninja -t targets | grep -q coverage-html ; then
      cd "$THIS_SCRIPT_PATH"
      clean_action
    fi
    cd "$THIS_SCRIPT_PATH"
  fi
}

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

check_missing_coverage_configuration

builder_describe_outputs \
  configure "${MESON_PATH}/build.ninja" \
  build "${MESON_PATH}/src/keyman-system-service"

if builder_has_option --coverage; then
  MESON_COVERAGE=-Db_coverage=true
else
  MESON_COVERAGE=
fi

builder_run_action clean       clean_action
builder_run_action configure   configure_action

[ -d "$MESON_PATH" ] && cd "$MESON_PATH"

builder_run_action build       ninja
builder_run_action test        test_action
builder_run_action install     ninja install
builder_run_action uninstall   ninja uninstall
