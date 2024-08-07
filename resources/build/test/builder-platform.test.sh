#!/usr/bin/env bash

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/builder.inc.sh"
# END STANDARD BUILD SCRIPT INCLUDE

. "${THIS_SCRIPT_PATH}/test-utils.inc.sh"

# Test overrides which disable win, delphi

BUILDER_PLATFORM_OVERRIDE=linux
BUILDER_TOOLS_OVERRIDE=

builder_describe \
  "Tests platform requirements" \
  build \
  :linux-project \
  :mac-project \
  :win-project \
  :delphi-project

builder_describe_platform \
  :linux-project linux \
  :mac-project mac \
  :win-project win \
  :delphi-project win,delphi

assert-equal "${_builder_targets[*]}" ":linux-project" "_builder_targets"
assert-equal "${!_builder_targets_excluded_by_platform[*]}" ":delphi-project :win-project :mac-project" "_builder_targets_excluded_by_platform"

# Test overrides with enable win, delphi

BUILDER_PLATFORM_OVERRIDE=win
BUILDER_TOOLS_OVERRIDE=(delphi)

builder_describe \
  "Tests platform requirements" \
  build \
  :linux-project \
  :mac-project \
  :win-project \
  :delphi-project

builder_describe_platform \
  :linux-project linux \
  :mac-project mac \
  :win-project win \
  :delphi-project win,delphi

assert-equal "${_builder_targets[*]}" ":win-project :delphi-project" "_builder_targets"
assert-equal "${!_builder_targets_excluded_by_platform[*]}" ":linux-project :mac-project" "_builder_targets_excluded_by_platform"
