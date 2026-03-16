#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "test wow64 kbdxx.dll loading" \
  clean configure build test \
  :x86 :x64 :arm64 :arm64ec

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"

WIN32_TARGET="bin/Win32/$TARGET_PATH/wow64kbd.exe"
X64_TARGET="bin/x64/$TARGET_PATH/wow64kbd.exe"
ARM64_TARGET="bin/ARM64/$TARGET_PATH/wow64kbd.exe"
ARM64EC_TARGET="bin/ARM64EC/$TARGET_PATH/wow64kbd.exe"

builder_describe_outputs \
  configure       /resources/build/win/delphi_environment_generated.inc.sh \
  build:x86       /windows/src/support/wow64kbd/$WIN32_TARGET \
  build:x64       /windows/src/support/wow64kbd/$X64_TARGET \
  build:arm64     /windows/src/support/wow64kbd/$ARM64_TARGET \
  build:arm64ec   /windows/src/support/wow64kbd/$ARM64EC_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  local Platform="$1"
  vs_msbuild wow64kbd.vcxproj -t:Clean "-p:Platform=${Platform}" -Verbosity:minimal
  clean_windows_project_files
}

function do_build() {
  local Platform="$1"
  vs_msbuild wow64kbd.vcxproj -t:Build "-p:Platform=${Platform}" -Verbosity:minimal
}

function do_test() {
  local Platform="$1"
  "./bin/${Platform}/${TARGET_PATH}/wow64kbd.exe"
}

function do_skip_test() {
  local Platform="$1"
  builder_echo "Skipping test for platform $1 because current processor architecture is ${PROCESSOR_ARCHITECTURE-unknown}"
}

builder_run_action clean:x86        do_clean Win32
builder_run_action clean:x64        do_clean x64
builder_run_action clean:arm64      do_clean ARM64
builder_run_action clean:arm64ec    do_clean ARM64EC
builder_run_action configure        configure_windows_build_environment
builder_run_action build:x86        do_build Win32
builder_run_action build:x64        do_build x64
builder_run_action build:arm64      do_build ARM64
builder_run_action build:arm64ex    do_build ARM64EC
builder_run_action test:x86         do_test Win32
builder_run_action test:x64         do_test x64

if [[ "${PROCESSOR_ARCHITECTURE-x}" == ARM64 ]]; then
  builder_run_action test:arm64       do_test ARM64
  builder_run_action test:arm64ec     do_test ARM64EC
else
  builder_run_action test:arm64       do_skip_test ARM64
  builder_run_action test:arm64ec     do_skip_test ARM64EC
fi
