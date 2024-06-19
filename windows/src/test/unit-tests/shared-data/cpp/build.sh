#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "shared data C++" clean configure build
builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/cppshareddata.exe"
X64_TARGET="$WIN64_TARGET_PATH/cppshareddata.exe"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /windows/src/test/unit-tests/shared-data/cpp/$WIN32_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  vs_msbuild cppshareddata.vcxproj //t:Clean
  clean_windows_project_files
}

function do_build() {
  vs_msbuild cppshareddata.vcxproj "//p:Platform=Win32"
  vs_msbuild cppshareddata.vcxproj "//p:Platform=x64"
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
