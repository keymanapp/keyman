# shellcheck shell=bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# Simple bash unit testing framework

assert-equal() {
  local actual="$1"
  local expected="$2"
  local variable=
  if [[ $# -gt 2 ]]; then
    variable="$3 "
  fi

  if [[ "${actual}" != "${expected}" ]]; then
    messages+=("- Actual ${variable}should equal expected:
                             Actual  : '${actual}'
                             Expected: '${expected}'

                          ")
    ((test_failures++))
  fi
}

assert-contains() {
  local actual="$1"
  local expected="$2"
  local variable=
  if [[ $# -gt 2 ]]; then
    variable="$3 "
  fi

  if [[ "${actual}" != *"${expected}"* ]]; then
    messages+=("- Actual ${variable}should contain expected:
                             Actual  : '${actual}'
                             Expected: '${expected}'

                          ")
    ((test_failures++))
  fi
}

assert-not-contains() {
  local actual="$1"
  local expected="$2"
  local variable=
  if [[ $# -gt 2 ]]; then
    variable="$3 "
  fi

  if [[ "${actual}" == *"${expected}"* ]]; then
    messages+=("- Actual ${variable}should not contain expected:
                             Actual  : '${actual}'
                             Expected: '${expected}'

                          ")
    ((test_failures++))
  fi
}

assert-failed() {
  local status="$1"

  if [[ "${status}" == 0 ]]; then
    messages+=("- Expected test to fail but it succeeded
                          ")
    ((test_failures++))
  fi
}

assert-succeeded() {
  local status="$1"

  if [[ "${status}" != 0 ]]; then
    messages+=("- Expected test to succeed but it failed
                          ")
    ((test_failures++))
  fi
}

_callfunc() {
  local command="$1"
  shift

  "${command}" "$@"
}

assert-true() {
  # shellcheck disable=SC2310
  if ! _callfunc "$@"; then
    messages+=("- Expected test to return true but it returned false
                          ")
    ((test_failures++))
  fi
}

assert-false() {
  # shellcheck disable=SC2310
  if _callfunc "$@"; then
    messages+=("- Expected test to return false but it returned true
                          ")
    ((test_failures++))
  fi
}

# setup() will run before each test
# can be overriden in test script
setup() {
  true
}

# teardown() will run after each test
# can be overriden in test script
teardown() {
  true
}

# setup_file() will run once before all tests in the file
# can be overriden in test script
setup_file() {
  true
}

# teardown_file() will run once after all tests in the file
# can be overriden in test script
teardown_file() {
  true
}

# Discover and run all tests in the current script
#
# Parameters:
#  --quiet                  optional first parameter. If specified suppress the
#                           output of the tests and save it in a logfile in
#                           /tmp/<testname>.log instead.
#  [$1 [$2]] / [first [last]] - optional range of lines in the tests script to search for tests.
#                           This helps to split a test file into several sections where
#                           each section can have separate setup/teardown functions.
#  [$3] / test_file         Name of the test file. Usually unset. Defaults to BASH_ARGV0.
#
# 'run_tests' looks for tests that follow the following pattern:
#   function test_name() {
#     test code
#   }
run_tests() {
  local test_count test_fail_count line func
  if [[ "${1:-}" == "--quiet" ]]; then
    test_suppress_output=true
    builder_echo green "(test logs are in /tmp/<testname>.log)"
    shift
  else
    test_suppress_output=false
  fi
  local first="${1:-1}"
  local last="${2:-$(wc -l < "${BASH_ARGV0}")}"
  local file="${3:-${BASH_ARGV0}}"

  test_count=0
  test_fail_count=0

  setup_file

  # shellcheck disable=SC2312
  while read -r line; do
    func="$(_get_test_func "${line}")"
    _run_single_test "${func}"
  done < <(sed -n "${first},${last}p" "${file}" | grep -e "^\s*function\s*test_")

  teardown_file

  builder_echo green "Total of ${test_count} tests run, $((test_count - test_fail_count)) passed, ${test_fail_count} failed"

  return "${test_fail_count}"
}

# Replace $FUNC with $MOCK
#
# Parameters:
#   $1 FUNC - function/command to mock
#   $2 MOCK - name of the mock function that replaces $FUNC
create_mock() {
  local FUNC="$1"
  local MOCK="${2:-}"
  local original_func_var="__original_${FUNC}"

  if [[ -z "${MOCK}" ]]; then
    MOCK="__default_mocked_${FUNC}"
    _create_default_mock_func "${FUNC}" "${MOCK}"
  fi

  if ! builtin declare -F "${original_func_var}" >/dev/null 2>&1; then
    # Save the original function
    case "$(builtin type -t "${FUNC}")" in
      function)
        builtin eval "${original_func_var}() $(builtin declare -f "${FUNC}" | tail -n +2)"
        ;;
      builtin)
        builtin eval "${original_func_var}() { builtin ${FUNC} \"\$@\"; }"
        ;;
      file)
        builtin eval "${original_func_var}() { $(builtin command -v "${FUNC}") \"\$@\"; }"
        ;;
      *)
        builder_echo error "Unhandled type for ${FUNC}: $(builtin type -t "${FUNC}")"
        return 1
        ;;
    esac
  fi

  builtin eval "${FUNC}() $(builtin declare -f "${MOCK}" | tail -n +2)"
}

# Remove the mock for $FUNC and replace it with the original implementation
#
# Parameters:
#   $1 FUNC - function/command to mock
unmock() {
  local FUNC="$1"
  local original_func_var="__original_${FUNC}"
  if builtin declare -f "${original_func_var}" >/dev/null 2>&1; then
    builtin eval "${FUNC}() $(builtin declare -f "${original_func_var}" | tail -n +2)"
  fi
}

# Create a mock function for $FUNC that simply outputs that it got called
#
# Parameters:
#   $1 FUNC - function/command to mock
#   $2 NAME - name of the mock function
_create_default_mock_func() {
  local FUNC="$1"
  local NAME="$2"
  builtin eval "${NAME}() { echo \"${FUNC} called\"; }"
}

_trim_start() {
  local string="$1"
  echo "${string#"${string%%[![:space:]]*}"}"
}

_trim_end() {
  local string="$1"
  echo "${string%"${string##*[![:space:]]}"}"
}

_get_test_func() {
  local line="$1"
  local name
  name=${line#function}
  name="$(_trim_start "${name}")"
  name=${name%\{};
  name="$(_trim_end "${name}")"
  name=${name%)}
  name="$(_trim_end "${name}")"
  name=${name%(}
  echo "${name}"
}

_call() {
  local func="$1"
  local testname="$2"
  if ${test_suppress_output}; then
    ${func} >> "/tmp/${testname}.log" 2>&1
  else
    ${func}
  fi
}

_test_wrapper() {
  local func="$1"
  _call "${func}" "${func}"
  return "${test_failures}"
}

_run_single_test() {
  local func="$1"
  declare -a messages=()

  # shellcheck disable=SC2154
  if ${_builder_debug_internal}; then
    builder_echo_debug "Running test ${func}"
  fi

  if ${test_suppress_output}; then
    echo "" > "/tmp/${func}.log"
  fi

  set +e
  test_failures=0
  ((test_count++))
  _call setup "${func}"

  # shellcheck disable=SC2310
  if _test_wrapper "${func}"; then
    _passed "${func}"
  else
    ((test_fail_count++))
    _failed "${func}"
  fi

  _call teardown "${func}"
  set -e
}

_passed() {
  local testname="$1"
  builder_echo green "  ✓ PASS: ${testname}"
}

_failed() {
  local testname="$1"
  builder_echo red "  ✕ FAIL: ${testname}
                           ${messages[*]}"
}
