#!/usr/bin/env bash

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
# END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

builder_describe \
  "Tests dependency builds" \
  "@./dep1" \
  "@./dep2 test" \
  "@./dep3 test:*" \
  "@./dep4 *:project" \
  "@./dep5 build:bar test:*" \
  "@./dep6 build:bar test" \
  "@./dep7:foo" \
  "@./dep8:foo test" \
  clean \
  configure \
  build \
  test \
  :project \
  :bar

builder_describe_outputs \
  configure  test.sh \
  build      non-existing-file \
  test       non-existing-file

builder_parse configure build test

function test_dep_should_build() {
  local at="$1"
  local dep="$2"

  if ! _builder_should_build_dep "$at" "resources/build/test/$dep"; then
    builder_die "FAIL: expecting to build dependency $dep for $at"
  else
    echo "PASS: will build dependency $dep for $at"
  fi
}

function test_dep_should_not_build() {
  local at="$1"
  local dep="$2"

  if _builder_should_build_dep "$at" "resources/build/test/$dep"; then
    builder_die "FAIL: not expecting to build dependency $dep for $at"
  else
    echo "PASS: will not build dependency $dep for $at"
  fi
}

test_dep_should_build configure:project dep1
test_dep_should_build build:project dep1
test_dep_should_build test:project dep1
test_dep_should_build configure:bar dep1
test_dep_should_build build:bar dep1
test_dep_should_build test:bar dep1

test_dep_should_not_build configure:project dep2
test_dep_should_not_build build:project dep2
test_dep_should_build test:project dep2
test_dep_should_not_build configure:bar dep2
test_dep_should_not_build build:bar dep2
test_dep_should_build test:bar dep2

test_dep_should_not_build configure:project dep3
test_dep_should_not_build build:project dep3
test_dep_should_build test:project dep3
test_dep_should_not_build configure:bar dep3
test_dep_should_not_build build:bar dep3
test_dep_should_build test:bar dep3

test_dep_should_build configure:project dep4
test_dep_should_build build:project dep4
test_dep_should_build test:project dep4
test_dep_should_not_build configure:bar dep4
test_dep_should_not_build build:bar dep4
test_dep_should_not_build test:bar dep4

test_dep_should_not_build configure:project dep5
test_dep_should_not_build build:project dep5
test_dep_should_build test:project dep5
test_dep_should_not_build configure:bar dep5
test_dep_should_build build:bar dep5
test_dep_should_build test:bar dep5

test_dep_should_not_build configure:project dep6
test_dep_should_not_build build:project dep6
test_dep_should_build test:project dep6
test_dep_should_not_build configure:bar dep6
test_dep_should_build build:bar dep6
test_dep_should_build test:bar dep6

test_dep_should_not_build clean:project dep1
test_dep_should_not_build clean:bar dep1
test_dep_should_not_build clean:project dep2
test_dep_should_not_build clean:bar dep2
test_dep_should_not_build clean:project dep3
test_dep_should_not_build clean:bar dep3
test_dep_should_not_build clean:project dep4
test_dep_should_not_build clean:bar dep4
test_dep_should_not_build clean:project dep5
test_dep_should_not_build clean:bar dep5
test_dep_should_not_build clean:project dep6
test_dep_should_not_build clean:bar dep6

test_dep_should_build     configure:project  dep7
test_dep_should_build     build:project      dep7
test_dep_should_build     test:project       dep7

# Test if 'build' actions are added because their output
# is missing
builder_parse test
if [[ "${_builder_chosen_action_targets[@]}" == "test:project test:bar build:project build:bar" ]]; then
  echo "PASS: 'build' actions automatically added"
else
  echo "All targets: ${_builder_chosen_action_targets[@]}"
  builder_die "FAIL: 'build' actions not automatically added, or unexpected action:targets added"
fi
