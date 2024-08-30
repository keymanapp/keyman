#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "texteditor" \
  clean configure build test

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/editor32.exe"
X64_TARGET="$X64_TARGET_PATH/editor64.exe"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /windows/src/support/texteditor/$WIN32_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  vs_msbuild Editor.vcxproj //t:Clean
  clean_windows_project_files
}

function do_build() {
  create-windows-output-folders
  # build_version.res
  vs_msbuild Editor.vcxproj //t:Build "//p:Platform=Win32"
  vs_msbuild Editor.vcxproj //t:Build "//p:Platform=x64"
  cp "$WIN32_TARGET" "$WINDOWS_PROGRAM_SUPPORT"
  cp "$X64_TARGET" "$WINDOWS_PROGRAM_SUPPORT"
  cp "$WIN32_TARGET_PATH/editor32.pdb" "$WINDOWS_DEBUGPATH_SUPPORT"
  cp "$X64_TARGET_PATH/editor64.pdb" "$WINDOWS_DEBUGPATH_SUPPORT"
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
