#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Text Services Framework Text Input Processor" \
  @/common/include \
  @/windows/src/engine/keymanmc \
  clean configure build test publish install

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/kmtip.dll"
X64_TARGET="$X64_TARGET_PATH/kmtip64.dll"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /windows/src/engine/kmtip/$X64_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  vs_msbuild kmtip.vcxproj //t:Clean
  clean_windows_project_files
}

function do_build() {
  create-windows-output-folders
  build_version.res
  vs_msbuild kmtip.vcxproj //t:Build "//p:Platform=Win32"
  vs_msbuild kmtip.vcxproj //t:Build "//p:Platform=x64"
  cp "$WIN32_TARGET" "$WINDOWS_PROGRAM_ENGINE"
  cp "$WIN32_TARGET_PATH/kmtip.pdb" "$WINDOWS_DEBUGPATH_ENGINE"
  cp "$X64_TARGET" "$WINDOWS_PROGRAM_ENGINE"
  cp "$X64_TARGET_PATH/kmtip64.pdb" "$WINDOWS_DEBUGPATH_ENGINE"
}

function do_publish() {
  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/kmtip.dll"
  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/kmtip64.dll"
  wrap-symstore "$WINDOWS_PROGRAM_ENGINE/kmtip.dll" //t keyman-engine-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_ENGINE/kmtip.pdb" //t keyman-engine-windows
  wrap-symstore "$WINDOWS_PROGRAM_ENGINE/kmtip64.dll" //t keyman-engine-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_ENGINE/kmtip64.pdb" //t keyman-engine-windows
}

function do_install() {
  cp "$WINDOWS_PROGRAM_ENGINE/kmtip.dll" "$INSTALLPATH_KEYMANENGINE/kmtip.dll"
  cp "$WINDOWS_PROGRAM_ENGINE/kmtip64.dll" "$INSTALLPATH_KEYMANENGINE/kmtip64.dll"
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action publish:project      do_publish
builder_run_action install:project      do_install
