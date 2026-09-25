#!/usr/bin/env bash
# shellcheck disable=2154 disable=1091

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder-basic.inc.sh"
# END STANDARD BUILD SCRIPT INCLUDE

. "${THIS_SCRIPT_PATH}/../../ci/trigger-definitions.inc.sh"
. "${THIS_SCRIPT_PATH}/../../ci/trigger-build-bot.inc.sh"
. "${THIS_SCRIPT_PATH}/../test-utils.inc.sh"
. "${KEYMAN_ROOT}/resources/build/jq.inc.sh"

# Initialize builder, no input parameters needed for these tests
builder_parse ""

readonly ALL_BUILD_PLATFORMS_SKIP_EXPECTED='[ios]="skip" [web]="skip" [linux]="skip" [common_mac]="skip" [common_windows]="skip" [mac]="skip" [windows]="skip" [common_web]="skip" [android]="skip" [developer]="skip" [common_linux]="skip"'
readonly ALL_BUILD_PLATFORMS_BUILD_EXPECTED='[ios]="build" [web]="build" [linux]="build" [common_mac]="build" [common_windows]="build" [mac]="build" [windows]="build" [common_web]="build" [android]="build" [developer]="build" [common_linux]="build"'

#----------------------------------------------------------------------------------------------------
# Test build bot message parsing (e2e)
#----------------------------------------------------------------------------------------------------

test_build_bot_check_messages() {
  builder_echo start test_build_bot_check_messages 'START TEST: build_bot_check_messages'

  # Mostly real data (only Build-bot commands edited for test)
  _do_test_build_bot_check_messages_file 14013 "[windows]=release" "${ALL_BUILD_PLATFORMS_SKIP_EXPECTED}"
  # Simplified data for testing Build-bot command sequences only
  _do_test_build_bot_check_messages_file 9999 "[windows]=release" '[windows]="skip" [developer]="release"'

  # test another sequence of commands
  _do_test_build_bot_check_messages_inline 8 "[windows]=release" '[common]="release" [windows]="skip" [developer]="build"' '{ "body": "Build-bot: skip windows\nBuild-bot: build developer\nBuild-bot: release common" }' '[]'
  _do_test_build_bot_check_messages_inline 9 "[windows]=release" '[common]="release" [windows]="skip" [developer]="build"' '{ "body": "Build-bot: skip:windows build:developer release:common" }' '[]'
  _do_test_build_bot_check_messages_inline 10 "[windows]=release" '[common]="release" [windows]="build"' '{ "body": "Build-bot: skip:windows build release:common" }' '[]'

  # Test some bad inputs
  _do_test_build_bot_check_messages_inline 1 "[windows]=release" '[windows]="release"' '{}' '[{ "sha": "1234", "commit": { "message": "maint(common): test\nBuild-bot: foo windows\n" }}]'
  # empty body, 'foo windows' in commit
  _do_test_build_bot_check_messages_inline 2 "[windows]=release" '[windows]="release"' '{  "body": "" }' '[{ "sha": "1234", "commit": { "message": "maint(common): test\nBuild-bot: foo windows\n" }}]'
  # attempts to escape jail
  _do_test_build_bot_check_messages_inline 3 "[windows]=release" '[windows]="release"' '{  "body": "Build-bot: '"'"'echo foo" }' '[{ "sha": "1234", "commit": { "message": "maint(common): test\n" }}]'
  _do_test_build_bot_check_messages_inline 4 "[windows]=release" '[windows]="release"' '{  "body": "Build-bot: echo *" }' '[{ "sha": "1234", "commit": { "message": "maint(common): test\n" }}]'
  _do_test_build_bot_check_messages_inline 5 "[windows]=release" '[windows]="release"' '{  "body": "Build-bot: \\0" }' '[{ "sha": "1234", "commit": { "message": "maint(common): test\n" }}]'
  # shellcheck disable=SC2016
  _do_test_build_bot_check_messages_inline 5 "[windows]=release" '[windows]="release"' '{  "body": "Build-bot: `echo escaped`" }' '[{ "sha": "1234", "commit": { "message": "maint(common): test\n" }}]'
  _do_test_build_bot_check_messages_inline 6 "[windows]=release" '[windows]="release"' '{  "body": "Build-bot: ' '[{ "sha": "1234", "commit": { "message": "maint(common): test\n" }}]'
  # incomplete command
  _do_test_build_bot_check_messages_inline 7 "[windows]=release" '[windows]="release"' '{  "body": "Build-bot: " }' '[{ "sha": "1234", "commit": { "message": "maint(common): test\n" }}]'

  builder_echo end test_build_bot_check_messages success 'SUCCESS: build_bot_check_messages'
}

#
# Test a set of Build-bot commands from a file
#
# Parameters:
#   1: PR number
#   2: Input build platforms based on touched files in PR
#   3: Expected build command
_do_test_build_bot_check_messages_file() {
  local prnum=$1
  eval "declare -gA build_platforms=($2)"
  eval "declare -A expected_build_platforms=($3)"

  # shellcheck disable=2312
  build_bot_check_messages "${prnum}" "$(cat "${THIS_SCRIPT_PATH}/pr-${prnum}-data.txt")" "$(cat "${THIS_SCRIPT_PATH}/pr-${prnum}-commits.txt")"

  for i in "${!expected_build_platforms[@]}"; do
    assert-equal "${build_platforms[${i}]:-missing}" "${expected_build_platforms[${i}]:-missing}" "PR #${prnum}: build_platforms[${i}]"
  done
  for i in "${!build_platforms[@]}"; do
    assert-equal "${build_platforms[${i}]:-missing}" "${expected_build_platforms[${i}]:-missing}" "PR #${prnum}: build_platforms[${i}]"
  done
}

#
# Test a set of Build-bot commands from parameters
#
# Parameters:
#   1: PR number
#   2: Input build platforms based on touched files in PR
#   3: Expected build command
#   4: JSON PR details (GitHub PR format)
#   5: JSON commit messages (GitHub PR format)
_do_test_build_bot_check_messages_inline() {
  local prnum=$1
  eval "declare -gA build_platforms=($2)"
  eval "declare -A expected_build_platforms=($3)"

  local prinfo="$4"
  local prcommits="$5"

  build_bot_check_messages "${prnum}" "${prinfo}" "${prcommits}"

  for i in "${!expected_build_platforms[@]}"; do
    assert-equal "${build_platforms[${i}]:-missing}" "${expected_build_platforms[${i}]:-missing}" "PR #${prnum}: build_platforms[${i}]"
  done
  for i in "${!build_platforms[@]}"; do
    assert-equal "${build_platforms[${i}]:-missing}" "${expected_build_platforms[${i}]:-missing}" "PR #${prnum}: build_platforms[${i}]"
  done
}

#----------------------------------------------------------------------------------------------------
# Test parsing of a single Build-bot command
#----------------------------------------------------------------------------------------------------

test_build_bot_update_commands() {
  builder_echo start test_build_bot_update_commands 'START TEST: _build_bot_update_commands'

  # added for #14565
  _do_test_build_bot_update_commands '[common_windows]="build" [windows]="build" [developer]="build"' "skip build:windows,developer" '[common_windows]="skip" [windows]="build" [developer]="build"'

  _do_test_build_bot_update_commands '[windows]="release"' "skip:windows" '[windows]="skip"'
  _do_test_build_bot_update_commands '[windows]="release"' "skip windows" '[windows]="skip"'
  _do_test_build_bot_update_commands '[windows]="release"' "skip:windows,developer" '[windows]="skip" [developer]="skip"'
  _do_test_build_bot_update_commands '[windows]="release"' "skip windows,developer" '[windows]="skip" [developer]="skip"'
  _do_test_build_bot_update_commands '[windows]="release"' "build:developer" '[windows]="release" [developer]="build"'
  _do_test_build_bot_update_commands '[windows]="release"' "build developer" '[windows]="release" [developer]="build"'
  _do_test_build_bot_update_commands '[windows]="release"' "skip:windows" '[windows]="skip"'
  _do_test_build_bot_update_commands '[windows]="release"' "skip windows" '[windows]="skip"'
  _do_test_build_bot_update_commands '[windows]="release"' "access:foo" '[windows]="release"'
  # not testing invalid legacy command: _do_test_build_bot_update_commands '[windows]="release"' "access foo" '[windows]="release"'
  _do_test_build_bot_update_commands '[windows]="release"' "build:foo" '[windows]="release"'
  # not testing invalid legacy command: _do_test_build_bot_update_commands '[windows]="release"' "build foo" '[windows]="release"'
  _do_test_build_bot_update_commands '[windows]="release"' "build:common" '[common]="build" [windows]="release"'
  _do_test_build_bot_update_commands '[windows]="release"' "build common" '[common]="build" [windows]="release"'
  _do_test_build_bot_update_commands '[windows]="release"' "build:all" "${ALL_BUILD_PLATFORMS_BUILD_EXPECTED}"
  _do_test_build_bot_update_commands '[windows]="release"' "build all" "${ALL_BUILD_PLATFORMS_BUILD_EXPECTED}"
  builder_echo end test_build_bot_update_commands success 'SUCCESS: build_bot_update_commands'
}

#
# Test a command
#
# Parameters:
#   1: starting build array based on PR files changed
#   2: build-bot command
#   3: expected finishing build array
#
_do_test_build_bot_update_commands() {
  eval "declare -gA build_platforms=($1)"
  local update_commands="$2"
  eval "declare -A expected_build_platforms=($3)"

  # shellcheck disable=SC2086  # intentionally no quotes
  _build_bot_update_commands ${update_commands}

  for i in "${!expected_build_platforms[@]}"; do
    assert-equal "${build_platforms[${i}]}" "${expected_build_platforms[${i}]}" "build_platforms[${i}]"
  done
  for i in "${!build_platforms[@]}"; do
    assert-equal "${build_platforms[${i}]}" "${expected_build_platforms[${i}]}" "build_platforms[${i}]"
  done
}

#----------------------------------------------------------------------------------------------------

test_build_bot_verify_platforms() {
  builder_echo start test_build_bot_verify_platforms 'START TEST: _build_bot_verify_platforms'
  _do_test_build_bot_verify_platforms "windows mac" "windows mac"
  _do_test_build_bot_verify_platforms "windows foo" "windows"
  _do_test_build_bot_verify_platforms "core" "core"
  _do_test_build_bot_verify_platforms "all" "${available_platforms[*]}"
  builder_echo end test_build_bot_verify_platforms success 'SUCCESS: _build_bot_verify_platforms'
}

_do_test_build_bot_verify_platforms() {
  # shellcheck disable=SC2206  # intentionally no quotes
  local platforms=($1)
  # shellcheck disable=SC2206  # intentionally no quotes
  local expected_platforms=($2)

  _build_bot_verify_platforms platforms

  assert-equal ${#platforms[@]} ${#expected_platforms[@]} "#platforms[@]"
  for i in "${!expected_platforms[@]}"; do
    assert-equal "${platforms[${i}]}" "${expected_platforms[${i}]}" "platforms[${i}]"
  done
}

#----------------------------------------------------------------------------------------------------

# If we have a Test-bot: skip, or no Test-bot: command, then we downgrade
# 'release' to a 'build'

test_test_bot_check_pr_body() {
  builder_echo start test_test_bot_check_pr_body 'START TEST: test_bot_check_pr_body'
  # Test-bot: skip means that we should only do a build+test, not a release build
  _do_test_test_bot_check_pr_body 1 '[windows]="release"' '{ "body": "Test-bot: skip" }' '[windows]="build"'
  # Empty body means that we should only do a build+test, not a release build
  _do_test_test_bot_check_pr_body 2 '[windows]="release"' '{ "body": "" }' '[windows]="build"'
  # '# User Testing' we will assume means there are unit tests
  _do_test_test_bot_check_pr_body 3 '[windows]="release"' '{ "body": "Some text\n# User Testing\n" }' '[windows]="release"'
  # Invalid test-bot command, treat as a skip
  _do_test_test_bot_check_pr_body 4 '[windows]="release"' '{ "body": "Test-bot: " }' '[windows]="build"'
  builder_echo end test_test_bot_check_pr_body success 'SUCCESS: test_bot_check_pr_body'
}

_do_test_test_bot_check_pr_body() {
  local prnum=$1
  eval "declare -gA build_platforms=($2)"
  local prinfo="$3"
  eval "declare -A expected_build_platforms=($4)"

  test_bot_check_pr_body "${prnum}" "${prinfo}"

  for i in "${!expected_build_platforms[@]}"; do
    assert-equal "${build_platforms[${i}]}" "${expected_build_platforms[${i}]}" "PR #${prnum}: build_platforms[${i}]"
  done
  for i in "${!build_platforms[@]}"; do
    assert-equal "${build_platforms[${i}]}" "${expected_build_platforms[${i}]}" "PR #${prnum}: build_platforms[${i}]"
  done
}

#----------------------------------------------------------------------------------------------------

test_is_skip_keyman_api_check() {
  builder_echo start test_is_skip_keyman_api_check 'START TEST: test_is_skip_keyman_api_check'
  result=$(is_skip_keyman_api_check 1 '{ "body": "Keyman-Api-Check: skip" }')
  assert-equal "${result}" "true" "PR #1: 'Keyman-Api-Check: skip' should return true"
  result=$(is_skip_keyman_api_check 2 '{ "body": "PR message" }')
  assert-equal "${result}" "false" "PR #2: omitted 'Keyman-Api-Check' should return false"
  result=$(is_skip_keyman_api_check 3 '{ "body": "Keyman-Api-Check: invalid" }')
  assert-equal "${result}" "false" "PR #3: invalid data for 'Keyman-Api-Check' should return false"
  result=$(is_skip_keyman_api_check 4 '{ "body": "Some message\nKeyman-Api-Check: skip" }')
  assert-equal "${result}" "true" "PR #4: 'Keyman-Api-Check: skip' at start of line should return true"
  result=$(is_skip_keyman_api_check 5 '{ "body": "Here Keyman-Api-Check: skip\nis in the middle of the line" }')
  assert-equal "${result}" "false" "PR #5: 'Keyman-Api-Check: skip' in the middle of the line should return false"
  builder_echo end test_is_skip_keyman_api_check success 'SUCCESS: test_is_skip_keyman_api_check'
}

#----------------------------------------------------------------------------------------------------

test_build_bot_verify_platforms
test_build_bot_update_commands
test_build_bot_check_messages
test_test_bot_check_pr_body
test_is_skip_keyman_api_check
