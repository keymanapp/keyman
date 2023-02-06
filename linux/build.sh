#!/usr/bin/env bash

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

################################ Main script ################################

# TODO: add keyman-config as child project

cd "$THIS_SCRIPT_PATH"

OUTPUT_PATH=

builder_describe \
  "Build Keyman for Linux." \
  "clean" \
  "configure" \
  "build+" \
  "test" \
  ":engine                   ibus-keyman" \
  "all                       clean, configure, and build" \
  "install                   install artifacts" \
  "uninstall                 uninstall artifacts" \
  "--debug,-d                Debug build" \
  "--output=OUTPUT_PATH,-o   Output path (default: build/)"

builder_parse "$@"

if builder_has_action all; then
  _builder_chosen_action_targets=("clean:engine" "configure:engine" "build:engine")
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

if builder_start_action clean:engine; then
  ibus-keyman/build.sh $builder_verbose $DEBUG_OPTION "${OUTPUT_OPTION[@]}" clean "${EXTRA_PARAMS[@]}"
  builder_finish_action success clean:engine
fi

if builder_start_action configure:engine; then
  ibus-keyman/build.sh $builder_verbose $DEBUG_OPTION "${OUTPUT_OPTION[@]}" configure "${EXTRA_PARAMS[@]}"
  builder_finish_action success configure:engine
fi

if builder_start_action build:engine; then
  ibus-keyman/build.sh $builder_verbose $DEBUG_OPTION "${OUTPUT_OPTION[@]}" build "${EXTRA_PARAMS[@]}"
  builder_finish_action success build:engine
fi

if builder_start_action test:engine; then
  ibus-keyman/build.sh $builder_verbose $DEBUG_OPTION "${OUTPUT_OPTION[@]}" test "${EXTRA_PARAMS[@]}"
  builder_finish_action success test:engine
fi

if builder_start_action install:engine; then
  ibus-keyman/build.sh $builder_verbose $DEBUG_OPTION "${OUTPUT_OPTION[@]}" install "${EXTRA_PARAMS[@]}"
  builder_finish_action success install:engine
fi

if builder_start_action uninstall:engine; then
  ibus-keyman/build.sh $builder_verbose $DEBUG_OPTION "${OUTPUT_OPTION[@]}" uninstall "${EXTRA_PARAMS[@]}"
  builder_finish_action success uninstall:engine
fi
