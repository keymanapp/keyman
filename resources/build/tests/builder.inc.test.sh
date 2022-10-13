#!/usr/bin/env bash

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../build-utils.sh"
# END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

# First up, test the simple case with a default :project target

builder_describe - clean build
builder_parse "build"
if [[ "${_builder_chosen_action_targets[@]}" != "build:project" ]]; then
  fail "  Test: builder_parse, shorthand form 'build' should give us 'build:project"
fi

if builder_start_action build; then
  echo "building project"
  builder_finish_action success build
else
  fail "FAIL: should have matched action build for :project"
fi

# Longhand builder_describe, builder_parse

builder_describe_parse_short_test() {
  local actions="$1"
  local targets="$2"
  local expected="$3"
  local parameters="$4"
  echo "Testing: builder_describe, parse \"$actions\" \"$targets\" $parameters"
  builder_describe "-" $actions $targets || fail "builder_describe died under curious circumstances"
  builder_parse $parameters || fail "builder_parse died under curious circumstances"
  if [[ "$expected" != "${_builder_chosen_action_targets[@]}" ]]; then
    fail "  Test: builder_describe, parse \"$actions\" \"$targets\" $parameters != \"$expected\""
  fi
}

builder_describe_parse_short_test "clean build test" ":module :tools :app" "build:module build:tools build:app" "build"
builder_describe_parse_short_test "clean build test" ":module :tools :app" "build:app" "build:app"
builder_describe_parse_short_test "clean build test" ":module :tools :app" "build:app clean:module" "build:app clean:module"
builder_describe_parse_short_test "clean build test" ":module :tools :app :project" "clean:module clean:tools clean:app clean:project build:app build:project" "clean build:app build:project"

# Test different default action

builder_describe "-" clean build test 'default+' :module :tools :app

if [[ $_builder_default_action != "default" ]]; then
  fail "FAIL: default action should have been 'default'"
fi

# Shorthand form where we don't have a :target (default is ":project")
if builder_start_action build; then
  echo "building project"
  builder_finish_action success build
else
  fail "FAIL: should have matched action build for :project"
fi

if builder_start_action clean:app; then
  echo "Cleaning <clean:app>"
  builder_finish_action success clean:app
else
  fail "FAIL: should have matched action clean for :app"
fi

if builder_start_action build:app; then
  echo "Building app"
  builder_finish_action success build:app
else
  fail "FAIL: should have matched action build for :app"
fi

if builder_start_action build:module; then
  fail "FAIL: should not have matched action build for :module"
fi

# "clean build test" ":app :engine" "--help"

builder_parse_test() {
  local expected="$1"
  local expected_options="$2"
  shift
  shift
  local parameters="$@"
  echo "${COLOR_BLUE}## Testing: builder_parse $parameters${COLOR_RESET}"
  builder_parse $parameters || fail "builder_parse died under curious circumstances"
  if [[ "$expected" != "${_builder_chosen_action_targets[@]}" ]]; then
    fail "  Test: builder_parse $parameters action:target != \"$expected\""
  fi
  if [[ "$expected_options" != "${_builder_chosen_options[@]}" ]]; then
    fail "  Test: builder_parse $parameters, options != \"$expected\""
  fi
}

builder_describe \
  "Tests the build-utils.sh builder functions. This is merely an example." \
  "clean        Cleans up any build artifacts" \
  "build        Do some building" \
  "test         Does some test stuff" \
  ":app" \
  ":engine      Thomas, y'know" \
  "--power,-p   Use powerful mode" \
  "--zoom,-z    Use zoom mode" \
  "--feature=FOO Enable feature foo"

# Test --options

builder_parse_test "clean:app test:engine" "--power" clean:app test:engine --power

if builder_has_option --power; then
  echo "PASS: --power option found"
else
  fail "FAIL: --power option not found"
fi

builder_parse_test "clean:app test:engine" "--zoom" clean:app test:engine -z

if builder_has_option --zoom; then
  echo "PASS: --zoom option found"
else
  fail "FAIL: --zoom option not found"
fi

# Test --feature <foo>

echo "${COLOR_BLUE}## Testing: builder_parse --feature xyzzy${COLOR_RESET}"
builder_parse --feature xyzzy

if builder_has_option --feature; then
  if [[ $FOO == xyzzy ]]; then
    echo "PASS: --feature option variable \$FOO has expected value 'xyzzy'"
  else
    fail "FAIL: --feature option variable \$FOO had value '$FOO' but should have had 'xyzzy'"
  fi
else
  echo "FAIL: --feature option not found"
fi

builder_parse -- one two "three four five"
if [[ ${builder_extra_params[0]} != "one" ]]; then
  fail "FAIL: -- extra parameter 'one' not found"
fi
if [[ ${builder_extra_params[1]} != "two" ]]; then
  fail "FAIL: -- extra parameter 'two' not found"
fi
if [[ ${builder_extra_params[2]} != "three four five" ]]; then
  fail "FAIL: -- extra parameter 'three four five' not found"
fi

# Run tests based in separate scripts to facilitate their operation

# Due to the nature of the build-utils-traps tests, only one may be
# specified at a time; each ends with an `exit`.
echo "${COLOR_BLUE}## Running trap tests${COLOR_RESET}"
$THIS_SCRIPT_PATH/build-utils-traps.test.sh error
$THIS_SCRIPT_PATH/build-utils-traps.test.sh error-in-function
$THIS_SCRIPT_PATH/build-utils-traps.test.sh incomplete
echo "${COLOR_BLUE}## Running dependency tests${COLOR_RESET}"
$THIS_SCRIPT_PATH/builder-deps.test.sh
echo "${COLOR_BLUE}## End external tests${COLOR_RESET}"
echo

# Finally, run with --help so we can see what it looks like
# Note:  calls `exit`, so no further tests may be defined.

echo "${COLOR_BLUE}## Testing --help${COLOR_RESET}"

builder_parse --no-color --help