#!/usr/bin/env bash
# Unit tests for verify_api.inc.sh
set -eu
shopt -s inherit_errexit

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-basic.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=2154
. "${KEYMAN_ROOT}/resources/build/test/testing-framework.inc.sh"
. "${THIS_SCRIPT%/*}/test.inc.sh"
. "${KEYMAN_ROOT}/linux/scripts/verify_api.inc.sh"


function setup_file() {
  SONAME=1
  LIB_NAME=libkeymancore
  PKG_NAME="${LIB_NAME}${SONAME}"
  EXIT_CODE=-1
}

function test__compare_versions__less_patch() {
  local result
  result="$(compare_versions "17.0.197" "17.0.198")"
  assert-equal "${result}" -1
}

function test__compare_versions__less_minor() {
  local result
  result="$(compare_versions "17.0.197" "17.1.197")"
  assert-equal "${result}" -1
}

function test__compare_versions__less_major() {
  local result
  result="$(compare_versions "17.0.197" "18.0.0")"
  assert-equal "${result}" -1
}

function test__compare_versions__greater() {
  local result
  result="$(compare_versions "17.0.198" "17.0.197")"
  assert-equal "${result}" 1
}

function test__compare_versions__same() {
  local result
  result="$(compare_versions "17.0.197" "17.0.197")"
  assert-equal "${result}" 0
}

function test__compare_versions__greater0() {
  local result
  result="$(compare_versions "17.0.197" "0")"
  assert-equal "${result}" 1
}

function test__get_highest_version_in_symbols_file() {
  local output
  createBase alpha
  echo ' (c++|optional)"typeinfo name for std::codecvt@Base" 17.0.244' >> linux/debian/libkeymancore1.symbols
  git add linux/debian/libkeymancore1.symbols
  git commit -m "API method added"

  # Execute
  output=$(get_highest_version_in_symbols_file "$(git rev-parse HEAD)")
  echo "${output[*]}" # for logging
  assert-equal "${output}" "17.0.244"
}

function test__check_updated_version_number__LineInsertedInBranch_OK() {
  local output
  createBase alpha

  local base_sha=$(git rev-parse master)

  # Add a line in chore branch
  sed -i 's/km_core_actions_dispose@Base 17.0.197/km_core_actions_dispose@Base 17.0.201/' linux/debian/libkeymancore1.symbols
  echo " km_core_foo@Base 17.0.200" >> linux/debian/libkeymancore1.symbols
  git add linux/debian/libkeymancore1.symbols
  git commit -m "API method added in chore branch"

  # Add a line in master branch
  git checkout master
  sed -i 's/km_core_actions_dispose@Base 17.0.197/km_core_actions_dispose@Base 17.0.199/' linux/debian/libkeymancore1.symbols
  git add linux/debian/libkeymancore1.symbols
  git commit -m "API method changed in master branch"
  echo "readme" > README.md
  git add README.md
  git commit -m "Some change on master"
  git checkout chore

  # merge master into chore
  git merge --strategy-option=ours master

  echo "## Calling API verification"
  pwd
  GIT_SHA="$(git rev-parse HEAD)"
  GIT_BASE="${base_sha}"

  output=$(check_updated_version_number)
  echo "${output[*]}" # for logging
  assert-contains "${output[*]}" "OK: libkeymancore1.symbols file got updated with package version number"
}

run_tests --quiet
