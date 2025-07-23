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
. "${KEYMAN_ROOT}/resources/locate_emscripten.inc.sh"
. "${KEYMAN_ROOT}/resources/build/test/testing-framework.inc.sh"

# Mocks

function mocked_builder_die() {
  echo "DIE: $*" 2>&1
  exit 99
}

# Setup/Teardown

function setup_file() {
  # create a temporary directory where we create simulated emsdk files/folders
  BASE_DIR=$(mktemp -d)

  # start with clean environment
  unset EMSCRIPTEN_BASE
  unset EMCC
}

function setup() {
  mkdir -p "${BASE_DIR}"
  create_mock  builder_die  mocked_builder_die
  create_mock  verify_emscripten_version
}

function teardown() {
  [[ -v EMSCRIPTEN_BASE ]] && rm -rf "${EMSCRIPTEN_BASE}"
  [[ -v EMCC ]] && rm -f "${EMCC}"
  unset EMSCRIPTEN_BASE
  unset EMCC
  rm -rf "${BASE_DIR:-}"
  unmock builder_die
  unmock verify_emscripten_version
}

# Tests

function test_locate_emscripten__with_EMSCRIPTEN_BASE_and_executable() {
  # Setup
  export EMSCRIPTEN_BASE="${BASE_DIR}/emsdk"
  mkdir -p "${EMSCRIPTEN_BASE}"
  touch "${EMSCRIPTEN_BASE}/emcc"
  chmod +x "${EMSCRIPTEN_BASE}/emcc"

  # Execute
  result=$(locate_emscripten 2>&1)

  # Verify
  assert-succeeded $?
  assert-equal "${result}" "verify_emscripten_version called"
}

function test_locate_emscripten__with_EMCC_set_and_executable() {
  # Setup
  export EMCC="${BASE_DIR}/emcc"
  touch "${EMCC}"
  chmod +x "${EMCC}"

  # Execute
  result=$(locate_emscripten 2>&1)

  # Verify
  assert-succeeded $?
  assert-equal "${result}" "verify_emscripten_version called"
}

function test_locate_emscripten__with_emcc_on_path() {
  # Setup
  export PATH="${BASE_DIR}:${PATH}"
  echo -e '#!/bin/sh\necho emcc called' > "${BASE_DIR}/emcc"
  chmod +x "${BASE_DIR}/emcc"

  # Execute
  result=$(locate_emscripten 2>&1)

  # Verify
  assert-succeeded $?
  assert-equal "${result}" "verify_emscripten_version called"
}

function test_locate_emscripten__not_found() {
  # Execute
  result=$(locate_emscripten 2>&1)

  # Verify
  assert-failed $?
  assert-contains "${result}" "Could not locate emscripten" "result"
}

function test_locate_emscripten__not_executable() {
  # Setup
  export EMCC="${BASE_DIR}/emcc"
  touch "${EMCC}"
  chmod -x "${EMCC}"

  # Execute
  result=$(locate_emscripten 2>&1)

  # Verify
  assert-failed $?
  assert-contains "${result}" "points to emcc but it is not executable" "result"
}

function test_locate_emscripten__with_EMSCRIPTEN_BASE_not_executable() {
  # Setup
  export EMSCRIPTEN_BASE="${BASE_DIR}/emsdk"
  mkdir -p "${EMSCRIPTEN_BASE}"
  touch "${EMSCRIPTEN_BASE}/emcc"
  chmod -x "${EMSCRIPTEN_BASE}/emcc"

  # Execute
  result=$(locate_emscripten 2>&1)

  # Verify
  assert-failed $?
  assert-contains "${result}" "contains emcc but it is not executable" "result"
}

function test_locate_emscripten__with_EMSCRIPTEN_BASE_missing_emcc() {
  # Setup
  export EMSCRIPTEN_BASE="${BASE_DIR}/emsdk"
  mkdir -p "${EMSCRIPTEN_BASE}"

  # Execute
  result=$(locate_emscripten 2>&1)

  # Verify
  assert-failed $?
  assert-contains "${result}" "does not point to emcc's folder" "result"
}

function test_locate_emscripten__with_EMSCRIPTEN_BASE_missing_dir() {
  # Setup
  EMSCRIPTEN_BASE="${BASE_DIR}/emsdk/upstream"
  mkdir -p "${EMSCRIPTEN_BASE}"
  export EMSCRIPTEN_BASE="${EMSCRIPTEN_BASE}/emscripten"

  # Execute
  result=$(locate_emscripten 2>&1)

  # Verify
  assert-failed $?
  assert-contains "${result}" "Variable EMSCRIPTEN_BASE (${EMSCRIPTEN_BASE}) points to a non-existent directory" "result"
}

function test_locate_emscripten__with_EMSCRIPTEN_BASE_nonexistent_but_emsdk_exists() {
  # Setup
  local EMSDK_DIR="${BASE_DIR}/emsdk"
  mkdir -p "${EMSDK_DIR}/upstream"
  export EMSCRIPTEN_BASE="${EMSDK_DIR}/upstream/emscripten"
  touch "${EMSDK_DIR}/emsdk"
  chmod +x "${EMSDK_DIR}/emsdk"

  # Execute
  result=$(locate_emscripten 2>&1)

  # Verify
  assert-succeeded $?
  assert-equal "${result}" "verify_emscripten_version called"
}

run_tests 1 "${LINENO}"

#------------------------------------------------------------------------------

NEW_START=${LINENO}

function setup() {
  mkdir -p "${BASE_DIR}"
}

function teardown() {
  unset EMSCRIPTEN_BASE
  unset KEYMAN_MIN_VERSION_EMSCRIPTEN
  unmock git
  unmock npm
  unset _builder_offline
  rm -rf "${BASE_DIR:-}"
}

function setup_file() {
  BASE_DIR=$(mktemp -d)
  teardown
}

function test_select_emscripten_version_with_emsdk__no_EMSCRIPTEN_BASE() {
  # Execute
  result=$(_select_emscripten_version_with_emsdk)

  # Verify
  assert-failed $?
  assert-contains "${result}" "Variable EMSCRIPTEN_BASE must be set"
}

function test_select_emscripten_version_with_emsdk__no_MIN_VERSION_EMSCRIPTEN() {
  # Setup
  local EMSCRIPTEN_DIR="${BASE_DIR}/emsdk/upstream"
  mkdir -p "${EMSCRIPTEN_DIR}"
  export EMSCRIPTEN_BASE="${EMSCRIPTEN_DIR}/emscripten"

  # Execute
  result=$(_select_emscripten_version_with_emsdk)

  # Verify
  assert-failed $?
  assert-contains "${result}" "Variable KEYMAN_MIN_VERSION_EMSCRIPTEN must be set"
}

function test_select_emscripten_version_with_emsdk__no_emsdk() {
  # Setup
  export EMSCRIPTEN_BASE="${BASE_DIR}/upstream/emscripten"
  mkdir -p "${EMSCRIPTEN_BASE}"
  KEYMAN_MIN_VERSION_EMSCRIPTEN="4.0.10"

  # Execute
  result=$(_select_emscripten_version_with_emsdk)

  # Verify
  assert-failed $?
  assert-contains "${result}" "emsdk[.bat] should be in ${BASE_DIR}"
}

function test_select_emscripten_version_with_emsdk__emsdk() {
  # Setup
  export EMSCRIPTEN_BASE="${BASE_DIR}/upstream/emscripten"
  mkdir -p "${EMSCRIPTEN_BASE}"
  KEYMAN_MIN_VERSION_EMSCRIPTEN="4.0.10"
  create_mock git
  create_mock npm
  echo -e '#!/bin/sh\necho "emsdk $* called"' > "${BASE_DIR}/emsdk"
  chmod +x "${BASE_DIR}/emsdk"
  _builder_offline=""

  # Execute
  result=$(_select_emscripten_version_with_emsdk)

  # Verify
  assert-succeeded $?
  assert-contains "${result}" "emsdk install 4.0.10 called"
  assert-contains "${result}" "git called"
  assert-contains "${result}" "npm called"
}

function test_select_emscripten_version_with_emsdk__EMSCRIPTEN_BASE_nonexistent_but_emsdk_exists() {
  # Setup
  local EMSDK_DIR="${BASE_DIR}/emsdk"
  mkdir -p "${EMSDK_DIR}/upstream"
  export EMSCRIPTEN_BASE="${EMSDK_DIR}/upstream/emscripten"
  KEYMAN_MIN_VERSION_EMSCRIPTEN="4.0.10"
  create_mock git
  create_mock npm
  echo -e '#!/bin/sh\nmkdir -p ${EMSCRIPTEN_BASE} ; echo "emsdk $* called"' > "${EMSDK_DIR}/emsdk"
  chmod +x "${EMSDK_DIR}/emsdk"
  _builder_offline=""

  # Execute
  result=$(_select_emscripten_version_with_emsdk)

  # Verify
  assert-succeeded $?
  assert-contains "${result}" "emsdk install 4.0.10 called"
  assert-contains "${result}" "git called"
  assert-contains "${result}" "npm called"
}

# TODO: write test for offline branch

run_tests "${NEW_START}" "${LINENO}"
