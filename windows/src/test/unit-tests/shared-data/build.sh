#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "test shared data consistency" \
  :cpp :delphi :test \
  clean configure build test

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/cppshareddata.exe"
X64_TARGET="$WIN64_TARGET_PATH/cppshareddata.exe"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /windows/src/test/unit-tests/shared-data/cpp/$WIN32_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_test() {
  builder_echo "Comparing structure sizes in x86, x64 between Delphi and C++"
  "./delphi/$WIN32_TARGET_PATH/delphishareddata.exe" > delphi-win32.txt
  "./delphi/$WIN64_TARGET_PATH/delphishareddata.exe" > delphi-win64.txt
  "./cpp/$WIN32_TARGET_PATH/cppshareddata.exe" > cpp-win32.txt
  "./cpp/$X64_TARGET_PATH/cppshareddata.exe" > cpp-win64.txt
  diff -q delphi-win32.txt delphi-win64.txt
  diff -q delphi-win32.txt cpp-win32.txt
  diff -q delphi-win32.txt cpp-win64.txt
}

builder_run_child_actions clean configure build
builder_run_action clean               rm -f delphi-win32.txt delphi-win64.txt cpp-win32.txt cpp-win64.txt
builder_run_action test:test           do_test
