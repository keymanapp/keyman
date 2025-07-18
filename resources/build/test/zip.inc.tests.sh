#!/usr/bin/env bash
# Unit tests for zip.inc.sh

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
# END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=2154
. "${KEYMAN_ROOT}/resources/build/zip.inc.sh"
. "${KEYMAN_ROOT}/resources/build/test/test-utils.inc.sh"

# Mock command -v to simulate presence/absence of zip
function command() {
  if [[ "$1" == "-v" && "$2" == "7z" ]]; then
    if [[ "${MOCK_ZIP_PRESENT:-0}" == "0" ]]; then
      echo "/usr/bin/7z"
      return 0
    else
      return 1
    fi
  fi
  builtin command "$@"
}

function setup() {
  # Mock zip and 7z commands to capture invocation
  ZIP_CMD_LOG=""
  function zip() {
    ZIP_CMD_LOG="zip $*"
    return 0
  }
  function 7z() {
    ZIP_CMD_LOG="7z $*"
    return 0
  }
  # Mock /custom/sevenz/7z.exe for Windows test
  function /custom/sevenz/7z.exe() {
    ZIP_CMD_LOG="/custom/sevenz/7z.exe $*"
    return 0
  }
}

function teardown() {
  unset zip
  unset 7z
  unset /custom/sevenz/7z.exe
  reset_zip_test_env
}

setup

# Helper to reset log and env
function reset_zip_test_env() {
  ZIP_CMD_LOG=""
  unset SEVENZ
  unset SEVENZ_HOME
  MOCK_ZIP_PRESENT=1
  export OSTYPE="linux"
}

function test_add_zip_files_with_zip_basic() {
  # Setup
  reset_zip_test_env
  MOCK_ZIP_PRESENT=1

  # Execute
  add_zip_files "archive.zip" file1.txt file2.txt

  # Verify
  assert-equal "${ZIP_CMD_LOG}" "zip archive.zip file1.txt file2.txt" "add_zip_files zip basic invocation"
}

function test_add_zip_files_with_zip_flags() {
  # Setup
  reset_zip_test_env
  MOCK_ZIP_PRESENT=1

  # Execute
  add_zip_files "archive.zip" -r -q -9 file1.txt

  # Verify
  # -q and -9 are passed to zip, not 7z
  assert-equal "${ZIP_CMD_LOG}" "zip -r -q -9 archive.zip file1.txt" "add_zip_files zip with flags"
}

function test_add_zip_files_with_zip_exclude_flag() {
  # Setup
  reset_zip_test_env
  MOCK_ZIP_PRESENT=1

  # Execute
  add_zip_files "archive.zip" -x@exclude.lst file1.txt

  # Verify
  assert-equal "${ZIP_CMD_LOG}" "zip -x@exclude.lst archive.zip file1.txt" "add_zip_files zip with exclude flag"
}

function test_add_zip_files_with_7z_basic() {
  # Setup
  reset_zip_test_env
  MOCK_ZIP_PRESENT=0

  # Execute
  add_zip_files "archive.7z" file1.txt file2.txt

  # Verify
  assert-equal "${ZIP_CMD_LOG}" "7z a archive.7z file1.txt file2.txt" "add_zip_files 7z basic invocation"
}

function test_add_zip_files_with_7z_flags() {
  # Setup
  reset_zip_test_env
  MOCK_ZIP_PRESENT=0

  # Execute
  add_zip_files "archive.7z" -r -q -5 file1.txt

  # Verify
  # -r is passed, -q becomes -bd -bb0, -5 becomes -mx5
  assert-equal "${ZIP_CMD_LOG}" "7z a -r -bd -bb0 -mx5 archive.7z file1.txt" "add_zip_files 7z with flags"
}

function test_add_zip_files_with_7z_exclude_flag() {
  # Setup
  reset_zip_test_env
  MOCK_ZIP_PRESENT=0

  # Execute
  add_zip_files "archive.7z" -x@exclude.lst file1.txt

  # Verify
  assert-equal "${ZIP_CMD_LOG}" "7z a -x@exclude.lst archive.7z file1.txt" "add_zip_files 7z with exclude flag"
}

function test_add_zip_files_with_7z_on_windows() {
  # Setup
  reset_zip_test_env
  MOCK_ZIP_PRESENT=0
  export OSTYPE="msys"
  export SEVENZ_HOME="/custom/sevenz"
  unset SEVENZ

  # Execute
  add_zip_files "archive.7z" file1.txt

  # Verify
  assert-equal "${ZIP_CMD_LOG}" "/custom/sevenz/7z.exe a archive.7z file1.txt" "add_zip_files 7z on windows"
}

function _create_files() {
  local FILEDIR="$1"
  mkdir -p "${FILEDIR}/a/b/c"
  touch "${FILEDIR}/file1.sh"
  touch "${FILEDIR}/file2.sh"
  touch "${FILEDIR}/build.sh"
  touch "${FILEDIR}/a/file3.sh"
  touch "${FILEDIR}/a/build.sh"
  touch "${FILEDIR}/a/b/file4.sh"
  touch "${FILEDIR}/a/b/build.sh"
  touch "${FILEDIR}/a/b/c/file5.sh"

  EXPECTED_CONTENT=".
./a
./a/b
./a/b/c
./a/b/c/file5.sh
./a/b/file4.sh
./a/file3.sh
./file1.sh
./file2.sh"
}

function test_add_zip_files_with_zip_real() {
  if ! builtin command -v zip >/dev/null 2>&1; then
    builder_echo warning "    IGNORE: add_zip_files zip with real data"
    return 0
  fi

  # Setup
  teardown
  MOCK_ZIP_PRESENT=1

  ARCHIVE=$(mktemp -u --suffix=.zip)
  FILEDIR=$(mktemp -d)
  _create_files "${FILEDIR}"

  # Execute
  (
    # shellcheck disable=2164
    cd "${FILEDIR}"
    add_zip_files "${ARCHIVE}" -r -q -xr!build.sh file1.sh file2.sh a/ # > /dev/null
  )

  # Verify
  NEWDIR=$(mktemp -d)
  (
    # shellcheck disable=2164
    cd "${NEWDIR}"
    unzip "${ARCHIVE}" > /dev/null
    ARCHIVE_CONTENT=$(find . | sort)
    assert-equal "${ARCHIVE_CONTENT}" "${EXPECTED_CONTENT}" "add_zip_files zip with real data" --quiet
  )

  rm -rf "${FILEDIR}"
  rm -rf "${NEWDIR}"
  rm "${ARCHIVE}"
  setup
}

function test_add_zip_files_with_7z_real() {
  if ! builtin command -v 7z >/dev/null 2>&1; then
    builder_echo warning "    IGNORE: add_zip_files 7z with real data"
    return 0
  fi

  # Setup
  teardown
  MOCK_ZIP_PRESENT=0

  ARCHIVE=$(mktemp -u --suffix=.zip)
  FILEDIR=$(mktemp -d)
  _create_files "${FILEDIR}"

  # Execute
  (
    # shellcheck disable=2164
    cd "${FILEDIR}"
    add_zip_files "${ARCHIVE}" -r -q -xr!build.sh file1.sh file2.sh a/ > /dev/null
    # 7z a -bd -bb0 "${ARCHIVE}" file1.sh file2.sh a/ -xr!build.sh
  )

  # Verify
  NEWDIR=$(mktemp -d)
  (
    # shellcheck disable=2164
    cd "${NEWDIR}"
    7z x "${ARCHIVE}" > /dev/null
    ARCHIVE_CONTENT=$(find . | sort)
    assert-equal "${ARCHIVE_CONTENT}" "${EXPECTED_CONTENT}" "add_zip_files 7z with real data" --quiet
  )

  rm -rf "${FILEDIR}"
  rm -rf "${NEWDIR}"
  rm "${ARCHIVE}"
  setup
}

# Run all tests
test_add_zip_files_with_zip_basic
test_add_zip_files_with_zip_flags
test_add_zip_files_with_zip_exclude_flag
test_add_zip_files_with_zip_real
test_add_zip_files_with_7z_basic
test_add_zip_files_with_7z_flags
test_add_zip_files_with_7z_exclude_flag
test_add_zip_files_with_7z_on_windows
test_add_zip_files_with_7z_real
