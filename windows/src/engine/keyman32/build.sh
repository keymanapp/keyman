#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Keystroke processing engine" \
  @/common/include \
  @/core \
  clean configure build test publish install \
  :x86 :x64

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/keyman32.dll"
X64_TARGET="$X64_TARGET_PATH/keyman64.dll"

builder_describe_outputs \
  configure    /resources/build/win/delphi_environment_generated.inc.sh \
  build:x86    /windows/src/engine/keyman32/$WIN32_TARGET \
  build:x64    /windows/src/engine/keyman32/$X64_TARGET \

#-------------------------------------------------------------------------------------------------------------------

function do_clean_x86() {
  vs_msbuild keyman32.vcxproj //t:Clean //p:Platform=Win32
  clean_windows_project_files
}

function do_clean_x64() {
  vs_msbuild keyman32.vcxproj //t:Clean //p:Platform=x64
  clean_windows_project_files
}

function do_build_x86() {
  create-windows-output-folders
  build_version.res
  vs_msbuild keyman32.vcxproj //t:Build "//p:Platform=Win32"
  cp "$WIN32_TARGET" "$WINDOWS_PROGRAM_ENGINE"
  cp "$WIN32_TARGET_PATH/keyman32.pdb" "$WINDOWS_DEBUGPATH_ENGINE"
}

function do_build_x64() {
  create-windows-output-folders
  run_in_vs_env rc version64.rc
  vs_msbuild keyman32.vcxproj //t:Build "//p:Platform=x64"
  cp "$X64_TARGET" "$WINDOWS_PROGRAM_ENGINE"
  cp "$X64_TARGET_PATH/keyman64.pdb" "$WINDOWS_DEBUGPATH_ENGINE"
}

function do_publish_x86() {
  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/keyman32.dll"
  wrap-symstore "$WINDOWS_PROGRAM_ENGINE/keyman32.dll" //t keyman-engine-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_ENGINE/keyman32.pdb" //t keyman-engine-windows
}

function do_publish_x64() {
  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/keyman64.dll"
  wrap-symstore "$WINDOWS_PROGRAM_ENGINE/keyman64.dll" //t keyman-engine-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_ENGINE/keyman64.pdb" //t keyman-engine-windows
}

function do_install_x86() {
  cp "$WINDOWS_PROGRAM_ENGINE/keyman32.dll" "$INSTALLPATH_KEYMANENGINE/keyman32.dll"
  echo "You may want to manually tweak keyman-debug-etw.man and fill in $INSTALLPATH_KEYMANENGINE/keyman32.dll"
  echo "and then run wevtutil im keyman-debug-etw.man to get the latest event tracing"
}

function do_install_x64() {
  cp "$WINDOWS_PROGRAM_ENGINE/keyman64.dll" "$INSTALLPATH_KEYMANENGINE/keyman64.dll"
}

builder_run_action clean:x86        do_clean_x86
builder_run_action clean:x64        do_clean_x64
builder_run_action configure        configure_windows_build_environment
builder_run_action build:x86        do_build_x86
builder_run_action build:x64        do_build_x64
# builder_run_action test:project         do_test
builder_run_action publish:x86      do_publish_x86
builder_run_action publish:x64      do_publish_x64
builder_run_action install:x86      do_install_x86
builder_run_action install:x64      do_install_x64
