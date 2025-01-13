#!/usr/bin/env bash
#
# Command line tests for kmc
#
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# . "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"
# . "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
# . "$KEYMAN_ROOT/developer/src/packages.inc.sh"

builder_describe "Test Keyman Keyboard Compiler kmc command line e2e" \
  "test" \
  "--verbose,-v          Verbose logging of output from kmc" \
  "--filter,-f=FILTER    Run tests that match FILTER" \
  ":basic                Basic sanity tests" \
  ":analyze              Test 'kmc analyze' command" \
  ":build                Test 'kmc build' command" \
  ":copy                 Test 'kmc copy' command'" \
  ":generate             Test 'kmc generate' command" \
  ":message              Test 'kmc message' command"

builder_parse "$@"

if [[ -z ${KMC+x} ]]; then
  KMC="node .."
fi

FAILED_TESTS=0
PASSED_TESTS=0
SKIPPED_TESTS=0

function run_kmc() {
  local EXPECTED_EXIT=$1
  local EXPECTED_MATCH="$2"
  shift 2
  local ACTUAL_EXIT=0
  local MATCH_RESULT=0
  local DATA

  if builder_has_option --filter; then
    if [[ ! "$@" =~ "$FILTER" ]]; then
      builder_echo blue "🟦 SKIP 'kmc $@'"
      SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
      return 0
    fi
  fi

  DATA=$($KMC "$@") || ACTUAL_EXIT=$?
  echo "$DATA" | grep -qF "$EXPECTED_MATCH" || MATCH_RESULT=$?

  if [[ $ACTUAL_EXIT != $EXPECTED_EXIT ]]; then
    builder_echo red "❌ FAIL 'kmc $@': expected exit code $EXPECTED_EXIT != actual exit code $ACTUAL_EXIT"
    builder_echo grey "Actual output from 'kmc $@':"
    echo "$DATA"
    FAILED_TESTS=$(($FAILED_TESTS + 1))
  elif [[ $MATCH_RESULT != 0 ]]; then
    builder_echo red "❌ FAIL 'kmc $@': expected string '$EXPECTED_MATCH' not found in output"
    builder_echo grey "Actual output from 'kmc $@':"
    echo "$DATA"
    FAILED_TESTS=$(($FAILED_TESTS + 1))
  else
    builder_echo "${COLOR_GREEN}✔${COLOR_RESET} PASS 'kmc $@'"
    if builder_has_option --verbose; then
      builder_echo grey "Actual output from 'kmc $@':"
      echo "$DATA"
    fi
    PASSED_TESTS=$(($PASSED_TESTS + 1))
  fi
}

function do_test_basic() {
  run_kmc 0 "Keyman Developer Command Line Interface" --help
  run_kmc 0 "kmc build [options] [command]" build --help
  run_kmc 1 "error: unknown option '--bad-option'" --bad-option
}

function build_cleanup() {
  rm -rf ./fixtures/command-line-tests/build/khmer_angkor/build/
}

function do_test_build() {
  # project/file builds
  build_cleanup
  run_kmc 0 "." build ./fixtures/command-line-tests/build/khmer_angkor/khmer_angkor.kpj

  build_cleanup
  run_kmc 0 "." build file ./fixtures/command-line-tests/build/khmer_angkor/khmer_angkor.kpj

  build_cleanup
  run_kmc 0 "." build ./fixtures/command-line-tests/build/khmer_angkor/

  # windows-package-installer tests
  # TODO: windows-package-installer is currently silent but should emit a 'success' message
  #build_cleanup
  run_kmc 0 "" build windows-package-installer \
    ./fixtures/command-line-tests/build/khmer_angkor/source/khmer_angkor.kps \
    --msi ./fixtures/command-line-tests/build/windows-installer/keymandesktop.txt \
    --exe ./fixtures/command-line-tests/build/windows-installer/setup.txt \
    --license ./fixtures/command-line-tests/build/windows-installer/license.txt \
    --out-file ./fixtures/command-line-tests/build/khmer_angkor/build/khmer_angkor.exe

  #build_cleanup
  run_kmc 1 "" build windows-package-installer \
    ./fixtures/command-line-tests/build/khmer_angkor/source/khmer_angkor.kps \
    --msi ./fixtures/command-line-tests/build/windows-installer/keymandesktop.txt \
    --exe ./fixtures/command-line-tests/build/windows-installer/setup.txt \
    --out-file ./fixtures/command-line-tests/build/khmer_angkor/build/khmer_angkor.exe
}

function do_test_analyze() {
  builder_heading "kmc analyze"
}

function do_test_copy() {
  builder_heading "kmc copy"
}

function do_test_generate() {
  builder_heading "kmc generate"
}

function do_test_message() {
  run_kmc 1 "Invalid parameter" message zap
  run_kmc 0 "ERROR_FileDoesNotExist" message 4003
}

builder_run_action test:basic    do_test_basic
builder_run_action test:analyze  do_test_analyze
builder_run_action test:build    do_test_build
builder_run_action test:copy     do_test_copy
builder_run_action test:generate do_test_generate
builder_run_action test:message  do_test_message

if builder_has_action test; then
  builder_echo grey "$PASSED_TESTS test(s) passed"
  builder_echo grey "$SKIPPED_TESTS test(s) skipped"
  if [[ $FAILED_TESTS != 0 ]]; then
    builder_echo red "$FAILED_TESTS test(s) failed"
    exit 1
  else
    builder_echo "0 test(s) failed"
  fi
fi
