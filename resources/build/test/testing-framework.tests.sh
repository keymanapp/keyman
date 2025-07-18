#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# Unit tests for locate_emscripten.inc.sh

shopt -s inherit_errexit

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
# END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/resources/build/test/testing-framework.inc.sh"

function myfunc() {
  echo "Hello world!"
}

function mocked_func() {
  echo "Hello mock!"
}

teardown() {
  unmock myfunc
  unmock command
  unmock more
  alias xyz=
}

function test_create_mock_function_mockfunc() {
  create_mock  myfunc  mocked_func
  status=$?

  assert-equal "$(myfunc)" "Hello mock!"
  assert-equal "${status}" "0"
}

function test_create_mock_function_defaultmock() {
  create_mock  myfunc
  status=$?

  assert-equal "$(myfunc)" "myfunc called"
  assert-equal "${status}" "0"
}

function test_create_mock_builtin_mockfunc() {
  create_mock  command  mocked_func
  status=$?

  assert-equal "$(command -v zip)" "Hello mock!"
  assert-equal "${status}" "0"
}

function test_create_mock_builtin_defaultmock() {
  create_mock  command
  status=$?

  assert-equal "$(command -v zip)" "command called"
  assert-equal "${status}" "0"
}

function test_create_mock_file_mockfunc() {
  create_mock  more  mocked_func
  status=$?

  assert-equal "$(more "${THIS_SCRIPT}")" "Hello mock!"
  assert-equal "${status}" "0"
}

function test_create_mock_file_defaultmock() {
  create_mock  more
  status=$?

  assert-equal "$(more "${THIS_SCRIPT}")" "more called"
  assert-equal "${status}" "0"
}

function test_create_mock_alias() {
  alias xyz=ls

  result=$(create_mock  xyz 2>&1)
  status=$?

  # For some reason we don't get "alias" in the output
  assert-contains "${result}" "Unhandled type for xyz:"
  assert-equal "${status}" "1"
}

function test_create_mock_keyword() {
  result=$(create_mock if 2>&1)
  status=$?

  assert-contains "${result}" "Unhandled type for if: keyword"
  assert-equal "${status}" "1"
}

function test_create_mock_nonexisting() {
  result=$(create_mock nonexisting 2>&1)
  status=$?

  assert-contains "${result}" "Unhandled type for nonexisting:"
  assert-equal "${status}" "1"
}

function test_unmock_function() {
  create_mock  myfunc
  assert-contains "$(declare -f myfunc)" "myfunc called"

  unmock myfunc
  status=$?

  assert-contains "$(declare -f myfunc)" "world"
}

function test_unmock_builtin() {
  create_mock  command
  assert-contains "$(declare -f command)" "command called"

  unmock command
  status=$?

  assert-contains "$(declare -f command)" "builtin command"
}

function test_unmock_file() {
  create_mock  more
  assert-contains "$(declare -f more)" "more called"

  unmock more
  status=$?

  assert-contains "$(declare -f more)" "/usr/bin/more"
}

run_tests
