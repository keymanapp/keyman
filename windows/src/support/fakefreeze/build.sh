#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Tell keyman.exe to pause, so Windows silently uninstalls the low level keyboard hook" \
  clean configure build test \
  :x86 :x64

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"

WIN32_TARGET="$WIN32_TARGET_PATH/fakefreeze.exe"
X64_TARGET="$X64_TARGET_PATH/fakefreeze.exe"

builder_describe_outputs \
  configure   /resources/build/win/delphi_environment_generated.inc.sh \
  build:x86   /windows/src/support/fakefreeze/$WIN32_TARGET \
  build:x64   /windows/src/support/fakefreeze/$X64_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  local Platform="$1"
  vs_msbuild fakefreeze.vcxproj //t:Clean "//p:Platform=${Platform}"
  clean_windows_project_files
}

function do_build() {
  local Platform="$1"
  vs_msbuild fakefreeze.vcxproj //t:Build "//p:Platform=${Platform}"
}

builder_run_action clean:x86      do_clean Win32
builder_run_action clean:x64      do_clean x64
builder_run_action configure      configure_windows_build_environment
builder_run_action build:x86      do_build Win32
builder_run_action build:x64      do_build x64
# builder_run_action test:x86      do_test Win32
# builder_run_action test:x64      do_test x64
