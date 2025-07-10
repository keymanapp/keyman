# shellcheck shell=bash

function assert-equal() {
  local actual="$1"
  local expected="$2"
  local message=
  local actual_to_show="'${actual}' "
  if [[ $# -gt 2 ]]; then
    message="$3: "
  fi
  if [[ $# -gt 3 ]]; then
    actual_to_show=""
  fi

  if [[ "${actual}" != "${expected}" ]]; then
    builder_die "  ✕ FAIL: ${message}actual result '${actual}' should equal expected '${expected}'"
  else
    builder_echo green "  ✓ PASS: ${message}result ${actual_to_show}is correct"
  fi
}
