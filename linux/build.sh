#!/usr/bin/env bash

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

################################ Main script ################################

OUTPUT_PATH=

builder_describe \
  "Build Keyman for Linux." \
  "clean" \
  "configure" \
  "build" \
  "test" \
  ":core                     Keyman Core" \
  ":engine                   ibus-keyman" \
  "default+                  clean, configure, and build" \
  "install                   install artifacts" \
  "uninstall                 uninstall artifacts" \
  "--debug,-d                Debug build" \
  "--output=OUTPUT_PATH,-o   Output path (default: build/)"

builder_parse "$@"

if builder_has_action default; then
  _builder_chosen_action_targets=("clean:core" "clean:engine" "configure:core" "build:core" "configure:engine" "build:engine")
fi

DEBUG_OPTION=
if builder_has_option --debug; then
  DEBUG_OPTION=--debug
fi

declare -a OUTPUT_OPTION
if builder_has_option --output; then
  OUTPUT_OPTION=("--output" "$OUTPUT_PATH")
fi

declare -a EXTRA_PARAMS
# shellcheck disable=SC2154 # assigned in builder.inc.sh
if (( ${#builder_extra_params[@]} > 0 )); then
  EXTRA_PARAMS=("--" "${builder_extra_params[@]}")
fi

if builder_start_action clean:core; then
  pushd ../core > /dev/null
  ./build.sh $builder_verbose $DEBUG_OPTION "${OUTPUT_OPTION[@]}" clean "${EXTRA_PARAMS[@]}"
  popd > /dev/null
  builder_finish_action success clean:core
fi

if builder_start_action clean:engine; then
  pushd ibus-keyman > /dev/null
  ./build.sh $builder_verbose $DEBUG_OPTION "${OUTPUT_OPTION[@]}" clean "${EXTRA_PARAMS[@]}"
  popd > /dev/null
  builder_finish_action success clean:engine
fi

if builder_start_action configure:core; then
  pushd ../core > /dev/null
  ./build.sh $builder_verbose $DEBUG_OPTION "${OUTPUT_OPTION[@]}" configure "${EXTRA_PARAMS[@]}"
  popd > /dev/null
  builder_finish_action success configure:core
fi

# We need to build core first before we can configure ibus-keyman
if builder_start_action build:core; then
  pushd ../core > /dev/null
  ./build.sh $builder_verbose $DEBUG_OPTION "${OUTPUT_OPTION[@]}" build "${EXTRA_PARAMS[@]}"
  popd > /dev/null
  builder_finish_action success build:core
fi

if builder_start_action configure:engine; then
  pushd ibus-keyman > /dev/null
  ./build.sh $builder_verbose $DEBUG_OPTION "${OUTPUT_OPTION[@]}" configure "${EXTRA_PARAMS[@]}"
  popd > /dev/null
  builder_finish_action success configure:engine
fi

if builder_start_action build:engine; then
  pushd ibus-keyman > /dev/null
  ./build.sh $builder_verbose $DEBUG_OPTION "${OUTPUT_OPTION[@]}" build "${EXTRA_PARAMS[@]}"
  popd > /dev/null
  builder_finish_action success build:engine
fi

if builder_start_action test:core; then
  pushd ../core > /dev/null
  ./build.sh $builder_verbose $DEBUG_OPTION "${OUTPUT_OPTION[@]}" test "${EXTRA_PARAMS[@]}"
  popd > /dev/null
  builder_finish_action success test:core
fi

if builder_start_action test:engine; then
  pushd ibus-keyman > /dev/null
  ./build.sh $builder_verbose $DEBUG_OPTION "${OUTPUT_OPTION[@]}" test "${EXTRA_PARAMS[@]}"
  popd > /dev/null
  builder_finish_action success test:engine
fi

if builder_start_action install:core; then
  pushd ../core > /dev/null
  ./build.sh $builder_verbose $DEBUG_OPTION "${OUTPUT_OPTION[@]}" install "${EXTRA_PARAMS[@]}"
  popd > /dev/null
  builder_finish_action success install:core
fi

if builder_start_action install:engine; then
  pushd ibus-keyman > /dev/null
  ./build.sh $builder_verbose $DEBUG_OPTION "${OUTPUT_OPTION[@]}" install "${EXTRA_PARAMS[@]}"
  popd > /dev/null
  builder_finish_action success install:engine
fi

if builder_start_action uninstall:core; then
  pushd ../core > /dev/null
  ./build.sh $builder_verbose $DEBUG_OPTION "${OUTPUT_OPTION[@]}" uninstall "${EXTRA_PARAMS[@]}"
  popd > /dev/null
  builder_finish_action success uninstall:core
fi

if builder_start_action uninstall:engine; then
  pushd ibus-keyman > /dev/null
  ./build.sh $builder_verbose $DEBUG_OPTION "${OUTPUT_OPTION[@]}" uninstall "${EXTRA_PARAMS[@]}"
  popd > /dev/null
  builder_finish_action success uninstall:engine
fi
