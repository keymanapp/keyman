#!/usr/bin/env bash

set -eu

# Avoid timing reports in unit tests
export _builder_timings=false

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-basic.inc.sh"
# END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"

cd "$THIS_SCRIPT_PATH"

#----------------------------------------------------------------------
# First up, test the simple case with a default :project target

builder_describe - clean build
builder_parse "build"
if [[ "${_builder_chosen_action_targets[@]}" != "build:project" ]]; then
  builder_die "  Test: builder_parse, shorthand form 'build' should give us 'build:project"
fi

if builder_start_action build; then
  echo "building project"
  builder_finish_action success build
else
  builder_die "FAIL: should have matched action build for :project"
fi

#----------------------------------------------------------------------
# Longhand builder_describe, builder_parse

builder_describe_parse_short_test() {
  local actions="$1"
  local targets="$2"
  local expected="$3"
  local parameters="$4"
  echo "Testing: builder_describe, parse \"$actions\" \"$targets\" $parameters"
  builder_describe "-" $actions $targets || builder_die "builder_describe died under curious circumstances"
  builder_parse $parameters || builder_die "builder_parse died under curious circumstances"
  if [[ "$expected" != "${_builder_chosen_action_targets[@]}" ]]; then
    builder_die "  Test: builder_describe, parse \"$actions\" \"$targets\" $parameters != \"$expected\""
  fi
}

builder_describe_parse_short_test "clean build test" ":module :tools :app" "build:module build:tools build:app" "build"
builder_describe_parse_short_test "clean build test" ":module :tools :app" "build:app" "build:app"
builder_describe_parse_short_test "clean build test" ":module :tools :app" "build:app clean:module" "build:app clean:module"
builder_describe_parse_short_test "clean build test" ":module :tools :app :project" "clean:module clean:tools clean:app clean:project build:app build:project" "clean build:app build:project"

#----------------------------------------------------------------------
# Test different default action

builder_describe "-" clean build test 'default+' :module :tools :app

if [[ $_builder_default_action != "default" ]]; then
  builder_die "FAIL: default action should have been 'default'"
fi

# Shorthand form where we don't have a :target (default is ":project")
if builder_start_action build; then
  echo "building project"
  builder_finish_action success build
else
  builder_die "FAIL: should have matched action build for :project"
fi

if builder_start_action clean:app; then
  echo "Cleaning <clean:app>"
  builder_finish_action success clean:app
else
  builder_die "FAIL: should have matched action clean for :app"
fi

if builder_start_action build:app; then
  echo "Building app"
  builder_finish_action success build:app
else
  builder_die "FAIL: should have matched action build for :app"
fi

if builder_start_action build:module; then
  builder_die "FAIL: should not have matched action build for :module"
fi

# "clean build test" ":app :engine" "--help"

builder_parse_test() {
  local expected="$1"
  local expected_options="$2"

  # When testing in a local environment, --debug will be added by default (see #11106)
  if [[ $KEYMAN_VERSION_ENVIRONMENT == "local" ]]; then
    expected_options="${expected_options} --debug"
  fi

  shift
  shift
  local parameters="$@"
  echo -e "${COLOR_BLUE}## Testing: builder_parse $parameters${COLOR_RESET}"
  builder_parse $parameters || builder_die "builder_parse died under curious circumstances"
  if [[ "$expected" != "${_builder_chosen_action_targets[@]}" ]]; then
    builder_die "  Test: builder_parse $parameters action:target != \"$expected\""
  fi
  if [[ "${expected_options}" != "${_builder_chosen_options[@]}" ]]; then
    builder_die "  Test: builder_parse $parameters, options != \"${expected_options}\""
  fi
}

builder_describe \
  "Tests the builder-basic.inc.sh builder functions. This is merely an example." \
  "clean        Cleans up any build artifacts" \
  "build        Do some building" \
  "test         Does some test stuff" \
  ":app" \
  ":engine      Thomas, y'know" \
  "--power,-p   Use powerful mode" \
  "--zoom,-z    Use zoom mode" \
  "--feature=FOO Enable feature foo" \
  "--bar=BAR    Enable bar" \
  "--baz=BAZ    Enable baz"

#----------------------------------------------------------------------
# Test --options

builder_parse_test "clean:app test:engine" "--power" clean:app test:engine --power

if builder_has_option --power; then
  builder_echo green "  ✓ PASS: --power option found"
else
  builder_die "FAIL: --power option not found"
fi

builder_parse_test "clean:app test:engine" "--zoom" clean:app test:engine -z

if builder_has_option --zoom; then
  builder_echo green "  ✓ PASS: --zoom option found"
else
  builder_die "FAIL: --zoom option not found"
fi

function verify_option() {
  local OPTIONNAME=$1
  local VARIABLE=$2
  local EXPECTED=$3

  if builder_has_option ${OPTIONNAME}; then
    if [[ ${!VARIABLE} == ${EXPECTED} ]]; then
      builder_echo green "  ✓ PASS: ${OPTIONNAME} option variable \$${VARIABLE} has expected value '${EXPECTED}'"
    else
      builder_die "FAIL: ${OPTIONNAME} option variable \$${VARIABLE} had value '${!VARIABLE}' but should have had '${EXPECTED}'"
    fi
  else
    builder_die "FAIL: ${OPTIONNAME} option not found"
  fi
}

#----------------------------------------------------------------------
# Test --feature <foo>

echo -e "${COLOR_BLUE}## Testing: builder_parse --feature xyzzy${COLOR_RESET}"
builder_parse --feature xyzzy

verify_option --feature FOO xyzzy

#----------------------------------------------------------------------
# Test --feature <foo> --bar <bar> --baz <baz>

echo -e "${COLOR_BLUE}## Testing: builder_parse --feature xyzzy --bar abc --baz def test${COLOR_RESET}"
builder_parse --feature xyzzy --bar abc --baz def test

verify_option --feature FOO xyzzy
verify_option --bar BAR abc
verify_option --baz BAZ def

builder_parse -- one two "three four five"
if [[ ${builder_extra_params[0]} != "one" ]]; then
  builder_die "FAIL: -- extra parameter 'one' not found"
fi
if [[ ${builder_extra_params[1]} != "two" ]]; then
  builder_die "FAIL: -- extra parameter 'two' not found"
fi
if [[ ${builder_extra_params[2]} != "three four five" ]]; then
  builder_die "FAIL: -- extra parameter 'three four five' not found"
fi

#----------------------------------------------------------------------
# Test handling of unknown options
echo -e "${COLOR_BLUE}## Running 'unknown options' tests${COLOR_RESET}"

# Test long form option with --builder-ignore-unknown-options
builder_parse --builder-ignore-unknown-options --some-other-option
if ! builder_ignore_unknown_options; then
  builder_die "FAIL: builder_ignore_unknown_options should be set"
fi
if [[ "${builder_ignored_options[@]}" != "--some-other-option" ]]; then
  builder_die "FAIL: --builder-ignore-unknown-options did not collect --some-other-option"
fi
builder_echo green "  ✓ PASS: --builder-ignore-unknown-options, --some-other-option was collected but ignored"

# Test short form option with --builder-ignore-unknown-options
builder_parse --builder-ignore-unknown-options -x
if ! builder_ignore_unknown_options; then
  builder_die "FAIL: builder_ignore_unknown_options should be set"
fi
if [[ "${builder_ignored_options[@]}" != "-x" ]]; then
  builder_die "FAIL: --builder-ignore-unknown-options '${builder_ignored_options[@]}' did not collect -x"
fi
builder_echo green "  ✓ PASS: --builder-ignore-unknown-options, -x was collected but ignored"

# Test long form option without --builder-ignore-unknown-options
(builder_parse --some-other-option >/dev/null) && EXIT_CODE=0 || EXIT_CODE=$?
if [[ $EXIT_CODE != 64 ]]; then
  builder_die "FAIL: without --builder-ignore-unknown-options, --some-other-option parameter should not have been recognized"
fi
builder_echo green "  ✓ PASS: without --builder-ignore-unknown-options, --some-other-option was not a recognized parameter"

# Test short form option without --builder-ignore-unknown-options
(builder_parse -x >/dev/null) && EXIT_CODE=0 || EXIT_CODE=$?
if [[ $EXIT_CODE != 64 ]]; then
  builder_die "FAIL: without --builder-ignore-unknown-options, -x parameter should not have been recognized"
fi
builder_echo green "  ✓ PASS: without --builder-ignore-unknown-options, -x was not a recognized parameter"

# Test that ignored options variables are correct without --builder-ignore-unknown-options
builder_parse test
if builder_ignore_unknown_options; then
  builder_die "FAIL: without --builder-ignore-unknown-options, builder_ignore_unknown_options() should not be set"
fi
if [[ ! -z "${builder_ignored_options[@]}" ]]; then
  builder_die "FAIL: without --builder-ignore-unknown-options, builder_ignored_options should be empty"
fi

#----------------------------------------------------------------------
# Test output of: --feature <foo> --bar <bar> --baz <baz> (#11676)
echo -e "${COLOR_BLUE}## Testing output of: builder_parse --feature xyzzy --bar abc --baz def test${COLOR_RESET}"
parse_output=$(builder_parse --feature xyzzy --bar abc --baz def test)
expected="$(builder_echo setmark "test.sh parameters: <--feature xyzzy --bar abc --baz def test>")"

if [[ $KEYMAN_VERSION_ENVIRONMENT == "local" ]]; then
  # When run in a local-dev environment, an extra line appears about the automatic --debug option.
  expected="$(builder_echo grey "Local build environment detected:  setting --debug")"$'\n'"$(builder_echo setmark "test.sh parameters: <--feature xyzzy --bar abc --baz def test --debug>")"
fi
if [[ "${parse_output[*]}" != "${expected}" ]]; then
  builder_die "FAIL: Wrong output for '--feature xyzzy --bar abc --baz def test':\n  Actual  : '${parse_output[*]}'\n  Expected: '${expected}'"
fi

#----------------------------------------------------------------------
function builder_echo_tests() {
  echo -e "${COLOR_BLUE}## Testing builder_echo (heading)${COLOR_RESET}"
  local _old_builder_debug_internal=${_builder_debug_internal}

  # regular build, no internal debug
  _builder_debug_internal=false
  expected="$(echo -e "${BUILDER_BOLD}${COLOR_BRIGHT_WHITE}[resources/build/test]${COLOR_RESET} ${COLOR_BLUE}description${COLOR_RESET}")"
  result=$(builder_echo heading "description")
  if [[ "${result[*]}" != "${expected}" ]]; then
    builder_die "FAIL: Wrong output for 'builder_echo heading description' (no debug):\n  Actual  : ${result[*]}\n  Expected: ${expected}"
  fi

  # regular build, with internal debug
  _builder_debug_internal=true
  expected="$(echo -e "${BUILDER_BOLD}${COLOR_BRIGHT_WHITE}[resources/build/test]${COLOR_RESET} ${COLOR_BLUE}description${COLOR_RESET}")"
  result=$(builder_echo heading "description")
  if [[ "${result[*]}" != "${expected}" ]]; then
    builder_die "FAIL: Wrong output for 'builder_echo start foo description' (debug):\n  Actual  : ${result[*]}\n  Expected: ${expected}"
  fi
  _builder_debug_internal=${_old_builder_debug_internal}
}

#----------------------------------------------------------------------
function builder_echo_start_end_tests() {
  echo -e "${COLOR_BLUE}## Testing builder_echo (start/end)${COLOR_RESET}"
  local _old_builder_debug_internal=${_builder_debug_internal}
  local _OLD_TEAMICITY_GIT_PATH=${TEAMCITY_GIT_PATH:-}
  local _old_builder_is_child=${_builder_is_child}

  # regular build, no internal debug, no child build
  TEAMCITY_GIT_PATH=""
  _builder_debug_internal=false
  _builder_is_child=1
  expected="$(builder_echo heading "description")"
  result=$(builder_echo start "foo" "description")
  if [[ "${result[*]}" != "${expected}" ]]; then
    builder_die "FAIL: Wrong output for 'builder_echo start foo description' (no debug, no child):\n  Actual  : ${result[*]}\n  Expected: ${expected}"
  fi
  expected="$(builder_echo success "description")"
  result=$(builder_echo end "foo" success "description")
  if [[ "${result[*]}" != "${expected}" ]]; then
    builder_die "FAIL: Wrong output for 'builder_echo end foo success description' (no debug, no child):\n  Actual  : ${result[*]}\n  Expected: ${expected}"
  fi
  expected="$(builder_echo error "description")"
  result=$(builder_echo end "foo" error "description")
  if [[ "${result[*]}" != "${expected}" ]]; then
    builder_die "FAIL: Wrong output for 'builder_echo end foo error description' (no debug, no child):\n  Actual  : ${result[*]}\n  Expected: ${expected}"
  fi

  # regular build, no internal debug, is child build
  TEAMCITY_GIT_PATH=""
  _builder_debug_internal=false
  _builder_is_child=0
  expected="$(builder_echo heading "description")"
  result=$(builder_echo start "foo" "description")
  if [[ "${result[*]}" != "${expected}" ]]; then
    builder_die "FAIL: Wrong output for 'builder_echo start foo description' (no debug, is child):\n  Actual  : ${result[*]}\n  Expected: ${expected}"
  fi
  expected="$(builder_echo success "description")"
  result=$(builder_echo end "foo" success "description")
  if [[ "${result[*]}" != "${expected}" ]]; then
    builder_die "FAIL: Wrong output for 'builder_echo end foo success description' (no debug, is child):\n  Actual  : ${result[*]}\n  Expected: ${expected}"
  fi
  expected="$(builder_echo error "description")"
  result=$(builder_echo end "foo" error "description")
  if [[ "${result[*]}" != "${expected}" ]]; then
    builder_die "FAIL: Wrong output for 'builder_echo end foo error description' (no debug, is child):\n  Actual  : ${result[*]}\n  Expected: ${expected}"
  fi

  # regular build, with internal debug, no child build
  _builder_debug_internal=true
  _builder_is_child=1
  expected="$(builder_echo heading "description")"
  result=$(builder_echo start "foo" "description")
  if [[ "${result[*]}" != "${expected}" ]]; then
    builder_die "FAIL: Wrong output for 'builder_echo start foo description' (debug, no child):\n  Actual  : ${result[*]}\n  Expected: ${expected}"
  fi
  expected="$(builder_echo success "description")"
  result=$(builder_echo end "foo" success "description")
  if [[ "${result[*]}" != "${expected}" ]]; then
    builder_die "FAIL: Wrong output for 'builder_echo end foo success description' (debug, no child):\n  Actual  : ${result[*]}\n  Expected: ${expected}"
  fi
  expected="$(builder_echo error "description")"
  result=$(builder_echo end "foo" error "description")
  if [[ "${result[*]}" != "${expected}" ]]; then
    builder_die "FAIL: Wrong output for 'builder_echo end foo error description' (debug, no child):\n  Actual  : ${result[*]}\n  Expected: ${expected}"
  fi

  # regular build, with internal debug, is child build
  _builder_debug_internal=true
  _builder_is_child=0
  expected="$(builder_echo heading "description")"
  result=$(builder_echo start "foo" "description")
  if [[ "${result[*]}" != "${expected}" ]]; then
    builder_die "FAIL: Wrong output for 'builder_echo start foo description' (debug, is child):\n  Actual  : ${result[*]}\n  Expected: ${expected}"
  fi
  expected="$(builder_echo success "description")"
  result=$(builder_echo end "foo" success "description")
  if [[ "${result[*]}" != "${expected}" ]]; then
    builder_die "FAIL: Wrong output for 'builder_echo end foo success description' (debug, is child):\n  Actual  : ${result[*]}\n  Expected: ${expected}"
  fi
  expected="$(builder_echo error "description")"
  result=$(builder_echo end "foo" error "description")
  if [[ "${result[*]}" != "${expected}" ]]; then
    builder_die "FAIL: Wrong output for 'builder_echo end foo error description' (debug, is child):\n  Actual  : ${result[*]}\n  Expected: ${expected}"
  fi

  # simulate running on TeamCity
  TEAMCITY_GIT_PATH="foo"
  # TC build, no internal debug
  _builder_debug_internal=false
  expected="##teamcity[blockOpened name='|[resources/build/test|] foo']
$(builder_echo heading "description")"
  result=$(builder_echo start "foo" "description")
  if [[ "${result[*]}" != "${expected}" ]]; then
    builder_die "FAIL: Wrong output for 'builder_echo start foo description' on TC (no debug):\n  Actual  : ${result[*]}\n  Expected: ${expected}"
  fi
  expected="$(builder_echo success "description")
##teamcity[blockClosed name='|[resources/build/test|] foo']"
  result=$(builder_echo end "foo" success "description")
  if [[ "${result[*]}" != "${expected}" ]]; then
    builder_die "FAIL: Wrong output for 'builder_echo end foo success description' on TC (no debug):\n  Actual  : ${result[*]}\n  Expected: ${expected}"
  fi
  expected="$(builder_echo error "description")
##teamcity[blockClosed name='|[resources/build/test|] foo']"
  result=$(builder_echo end "foo" error "description")
  if [[ "${result[*]}" != "${expected}" ]]; then
    builder_die "FAIL: Wrong output for 'builder_echo end foo error description' on TC (no debug):\n  Actual  : ${result[*]}\n  Expected: ${expected}"
  fi

  # TC build, with internal debug
  _builder_debug_internal=true
  expected="##teamcity[blockOpened name='|[resources/build/test|] foo']
$(builder_echo heading "description")"
  result=$(builder_echo start "foo" "description")
  if [[ "${result[*]}" != "${expected}" ]]; then
    builder_die "FAIL: Wrong output for 'builder_echo start foo description' on TC (debug):\n  Actual  : ${result[*]}\n  Expected: ${expected}"
  fi
  expected="$(builder_echo success "description")
##teamcity[blockClosed name='|[resources/build/test|] foo']"
  result=$(builder_echo end "foo" success "description")
  if [[ "${result[*]}" != "${expected}" ]]; then
    builder_die "FAIL: Wrong output for 'builder_echo end foo success description' on TC (debug):\n  Actual  : ${result[*]}\n  Expected: ${expected}"
  fi
  expected="$(builder_echo error "description")
##teamcity[blockClosed name='|[resources/build/test|] foo']"
  result=$(builder_echo end "foo" error "description")
  if [[ "${result[*]}" != "${expected}" ]]; then
    builder_die "FAIL: Wrong output for 'builder_echo end foo error description' on TC (debug):\n  Actual  : ${result[*]}\n  Expected: ${expected}"
  fi
  TEAMCITY_GIT_PATH="${_OLD_TEAMICITY_GIT_PATH}"
  _builder_debug_internal=${_old_builder_debug_internal}
  _builder_is_child=${_old_builder_is_child}
}
builder_echo_tests
builder_echo_start_end_tests

# Run tests based in separate scripts to facilitate their operation

#----------------------------------------------------------------------

# Due to the nature of the build-utils-traps tests, only one may be
# specified at a time; each ends with an `exit`.
echo -e "${COLOR_BLUE}## Running trap tests${COLOR_RESET}"
$THIS_SCRIPT_PATH/build-utils-traps.test.sh error
$THIS_SCRIPT_PATH/build-utils-traps.test.sh error-in-function
$THIS_SCRIPT_PATH/build-utils-traps.test.sh incomplete

echo -e "${COLOR_BLUE}## Running dependency tests${COLOR_RESET}"
"$THIS_SCRIPT_PATH/builder-deps.test.sh"
"$THIS_SCRIPT_PATH/dependencies/test.sh"
"$THIS_SCRIPT_PATH/trees/test.sh"
"$THIS_SCRIPT_PATH/debug-deps/test.sh"
"$THIS_SCRIPT_PATH/ignored-flags/test.sh"

echo -e "${COLOR_BLUE}## Test builder.inc.sh platform and tool constraints${COLOR_RESET}"
"$THIS_SCRIPT_PATH/builder-platform.test.sh"


echo -e "${COLOR_BLUE}## Test builder.inc.sh 'builder-style' script${COLOR_RESET}"
./builder-invalid-script.test.sh || builder_die "FAIL: builder-invalid-script.test.sh returned failure code $?"

echo -e "${COLOR_BLUE}## End external tests${COLOR_RESET}"
echo

(
  # Finally, run with --help so we can see what it looks like; note:
  # builder_parse calls `exit 0` on a --help run, so running in a subshell
  echo -e "${COLOR_BLUE}## Testing --help${COLOR_RESET}"
  builder_parse --no-color --help --no-timings
) || builder_die "FAIL: builder-parse unexpectedly returned failure code $?"

echo -e "${COLOR_GREEN}======================================================${COLOR_RESET}"
echo -e "${COLOR_GREEN}All tests passed successfully${COLOR_RESET}"
echo -e "${COLOR_GREEN}======================================================${COLOR_RESET}"
