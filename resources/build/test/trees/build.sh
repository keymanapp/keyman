#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/build-utils.sh"
# END STANDARD BUILD SCRIPT INCLUDE

# Test builder_describe_outputs and dependencies

builder_describe "parent test module" \
  :child1 \
  :child2 \
  :child3=child3_renamed/src \
  clean \
  configure \
  build \
  test \
  install \
  error

builder_parse "$@"

if builder_is_child_build; then
  builder_die "FAIL: builder_is_child_build should be false but was $_builder_is_child for the parent script"
else
  builder_echo "PASS: builder_is_child_build is false ($_builder_is_child) for the parent script"
fi

# All child actions will generate files which we need to verify for test
rm -f ./child?.* ./dep.clean ./dep.configure ./dep.build

function test_present() {
  local target=$1
  local action=$2

  local CHECK="${COLOR_GREEN}✔${COLOR_RESET}" # ✔
  local CROSS="${COLOR_RED}❌${COLOR_RESET}" # ❌

  if builder_has_action $action:$target; then
    if [ ! -f $target.$action ]; then
      builder_die "$CROSS FAIL: ./$target.$action to be present"
    else
      echo -e "$CHECK PASS: ./$target.$action found as expected"
    fi
  fi
}

# We need to specify the order to run actions in the parent script
#
# This may looks as simple as:
#
#     builder_run_child_actions clean configure build test install
#
builder_run_child_actions clean
test_present child1 clean
test_present child2 clean
test_present child3 clean

builder_run_child_actions configure
test_present child1 configure
test_present child2 configure
test_present child3 configure

# Test chaining commands in a single statement
builder_run_child_actions build test
test_present child1 build
test_present child2 build
test_present child3 build
test_present child1 test
test_present child2 test
test_present child3 test

# We are customising the order in which child scripts are installed, so 2 goes before 1
builder_run_child_actions install:child2
test_present child2 install

builder_run_child_actions install:child1
test_present child1 install

builder_run_child_actions error

echo Done