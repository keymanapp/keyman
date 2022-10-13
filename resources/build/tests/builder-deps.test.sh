#!/usr/bin/env bash

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../build-utils.sh"
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
  configure \
  build \
  test \
  :project \
  :bar

builder_describe_outputs \
  configure  builder.inc.test.sh \
  build      non-existing-file \
  test       non-existing-file

builder_parse configure build test

function test_dep_should_build() {
  local at="$1"
  local dep="$2"

  if ! _builder_should_build_dep "$at" "resources/build/tests/$dep"; then
    fail "FAIL: expecting to build dependency $dep for $at"
  else
    echo "PASS: will build dependency $dep for $at"
  fi
}

function test_dep_should_not_build() {
  local at="$1"
  local dep="$2"

  if _builder_should_build_dep "$at" "resources/build/tests/$dep"; then
    fail "FAIL: not expecting to build dependency $dep for $at"
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
