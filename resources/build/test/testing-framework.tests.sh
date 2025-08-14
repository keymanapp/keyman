#!/usr/bin/env bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# Unit tests for locate_emscripten.inc.sh

# shellcheck disable=SC2317

shopt -s inherit_errexit

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-basic.inc.sh"
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

function test__create_mock__function_mockfunc() {
  create_mock  myfunc  mocked_func
  status=$?

  assert-equal "$(myfunc)" "Hello mock!"
  assert-equal "${status}" "0"
}

function test__create_mock__function_defaultmock() {
  create_mock  myfunc
  status=$?

  assert-equal "$(myfunc)" "myfunc called"
  assert-equal "${status}" "0"
}

function test__create_mock__builtin_mockfunc() {
  create_mock  command  mocked_func
  status=$?

  assert-equal "$(command -v zip)" "Hello mock!"
  assert-equal "${status}" "0"
}

function test__create_mock__builtin_defaultmock() {
  create_mock  command
  status=$?

  assert-equal "$(command -v zip)" "command called"
  assert-equal "${status}" "0"
}

function test__create_mock__file_mockfunc() {
  create_mock  more  mocked_func
  status=$?

  assert-equal "$(more "${THIS_SCRIPT}")" "Hello mock!"
  assert-equal "${status}" "0"
}

function test__create_mock__file_defaultmock() {
  create_mock  more
  status=$?

  assert-equal "$(more "${THIS_SCRIPT}")" "more called"
  assert-equal "${status}" "0"
}

function test__create_mock__alias() {
  alias xyz=ls

  result=$(create_mock  xyz 2>&1)
  status=$?

  # For some reason we don't get "alias" in the output
  assert-contains "${result}" "Unhandled type for xyz:"
  assert-equal "${status}" "1"
}

function test__create_mock__keyword() {
  result=$(create_mock if 2>&1)
  status=$?

  assert-contains "${result}" "Unhandled type for if: keyword"
  assert-equal "${status}" "1"
}

function test__create_mock__nonexisting() {
  result=$(create_mock nonexisting 2>&1)
  status=$?

  assert-contains "${result}" "Unhandled type for nonexisting:"
  assert-equal "${status}" "1"
}

function test__unmock__function() {
  create_mock  myfunc
  assert-contains "$(declare -f myfunc)" "myfunc called"

  unmock myfunc
  status=$?

  assert-contains "$(declare -f myfunc)" "world"
}

function test__unmock__builtin() {
  create_mock  command
  assert-contains "$(declare -f command)" "command called"

  unmock command
  status=$?

  assert-contains "$(declare -f command)" "builtin command"
}

function test__unmock__file() {
  create_mock  more
  assert-contains "$(declare -f more)" "more called"

  unmock more
  status=$?

  assert-contains "$(declare -f more)" "/usr/bin/more"
}

function test__run_tests__reports_failures() {
  local test_output test_output_contents
  test_output=$(mktemp)
  LAST_LINE="$(wc -l < "${BASH_ARGV0}")"

  # Start a new shell that will run the tests
  bash - > "${test_output}" <<EOF
    . "$0" --recursive

    # ${LAST_LINE} and ${BASH_ARGV0} intentionally not escaped!
    run_tests "\${START_LAST_SECTION}" "${LAST_LINE}" "${BASH_ARGV0}"
    status=\$?

    unmock builder_echo
    exit ${status}
EOF
  status=$?

  test_output_contents="$(cat "${test_output}")"
  rm -f "${test_output}"

  assert-equal "${status}" 2
  assert-contains "${test_output_contents}" "Total of 3 tests run, 1 passed, 2 failed"
  assert-contains "${test_output_contents}" "FAIL: test_failing1"
  assert-contains "${test_output_contents}" "FAIL: test_failing2"
  assert-contains "${test_output_contents}" "PASS: test_succeeding1"
}


# shellcheck disable=SC2031
if [[ "${1:-}" != "--recursive" ]]; then
  run_tests 1 "${LINENO}"
  exit
fi

#------------------------------------------------------------------------------
# Tests that are run as part of 'test__run_tests__reports_failures'

# shellcheck disable=SC2034
START_LAST_SECTION="${LINENO}"

function test_failing1() {
  assert-failed 0
}

function test_failing2() {
  assert-succeeded 1
}

function test_succeeding1() {
  assert-succeeded 0
}
