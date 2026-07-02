#!/usr/bin/env bash

set -eu

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder-basic.inc.sh"
# END STANDARD BUILD SCRIPT INCLUDE

cd "$THIS_SCRIPT_PATH"

function _before_test() {
  rm -f child?.* dep.*
}


builder_heading "--- Full clean configure build install test build ---"
_before_test
./build.sh clean configure build install test
if [ $(ls child?.* | wc -l) -ne 14 ]; then
  builder_die "unexpected output file count"
fi

builder_heading "--- Test error result for child build ---"
_before_test
./build.sh error && (echo should not have passed; exit 1) || (echo "  ... build returned error as expected")

builder_heading "--- Test building only specific actions and targets (install:child1 test:child2) ---"
_before_test
./build.sh install:child1 test:child2
if [ $(ls child?.* | wc -l) -ne 2 ]; then
  builder_die "unexpected output file count"
fi

builder_heading "--- Test that --no-deps is passed correctly to child scripts ---"
_before_test
./build.sh --no-deps
if [ -f ./dep.configure ] || [ -f ./dep.build ]; then
  builder_die "unexpected dep.configure or dep.build file, should not have built this because of --no-deps flag"
fi

builder_heading "--- Test that 'nothing' does not automatically run 'build on children ---"
_before_test
./build.sh nothing
if [ -f ./dep.configure ] || [ -f ./dep.build ] || compgen -G './child?.build' > /dev/null; then
  builder_die "unexpected dep.configure, dep.build, or child?.build file, should not have built this because of 'nothing' action"
fi
