#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Helper app to refresh Windows language settings" \
  @/common/include \
  clean configure build test publish install

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/kmrefresh.x86.exe"
X64_TARGET="$X64_TARGET_PATH/kmrefresh.x64.exe"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /windows/src/engine/kmrefresh/$X64_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  vs_msbuild kmrefresh.vcxproj //t:Clean
  clean_windows_project_files
}

function do_build() {
  create-windows-output-folders
  build_version.res
  vs_msbuild kmrefresh.vcxproj //t:Build "//p:Platform=Win32"
  vs_msbuild kmrefresh.vcxproj //t:Build "//p:Platform=x64"
  cp "$WIN32_TARGET" "$WINDOWS_PROGRAM_ENGINE"
  cp "$WIN32_TARGET_PATH/kmrefresh.x86.pdb" "$WINDOWS_DEBUGPATH_ENGINE"
  cp "$X64_TARGET" "$WINDOWS_PROGRAM_ENGINE"
  cp "$X64_TARGET_PATH/kmrefresh.x64.pdb" "$WINDOWS_DEBUGPATH_ENGINE"
}

function do_publish() {
  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/kmrefresh.x86.exe"
  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/kmrefresh.x64.exe"
  wrap-symstore "$WINDOWS_PROGRAM_ENGINE/kmrefresh.x86.exe" //t keyman-engine-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_ENGINE/kmrefresh.x86.pdb" //t keyman-engine-windows
  wrap-symstore "$WINDOWS_PROGRAM_ENGINE/kmrefresh.x64.exe" //t keyman-engine-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_ENGINE/kmrefresh.x64.pdb" //t keyman-engine-windows
}

function do_install() {
  cp "$WINDOWS_PROGRAM_ENGINE/kmrefresh.x86.exe" "$INSTALLPATH_KEYMANENGINE/kmrefresh.x86.exe"
  cp "$WINDOWS_PROGRAM_ENGINE/kmrefresh.x64.exe" "$INSTALLPATH_KEYMANENGINE/kmrefresh.x64.exe"
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action publish:project      do_publish
builder_run_action install:project      do_install
