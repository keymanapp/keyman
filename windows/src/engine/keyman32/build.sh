#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Keystroke processing engine (32 bit)" \
  @/common/include \
  @/core:x86 \
  clean configure build test publish install

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/keyman32.dll"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /windows/src/engine/keyman32/$WIN32_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  vs_msbuild keyman32.vcxproj //t:Clean
  clean_windows_project_files
}

function do_build() {
  create-windows-output-folders
  build_version.res
  vs_msbuild keyman32.vcxproj //t:Build "//p:Platform=Win32"
  cp "$WIN32_TARGET" "$WINDOWS_PROGRAM_ENGINE"
  cp "$WIN32_TARGET_PATH/keyman32.pdb" "$WINDOWS_DEBUGPATH_ENGINE"
}

function do_publish() {
  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/keyman32.dll"
  wrap-symstore "$WINDOWS_PROGRAM_ENGINE/keyman32.dll" //t keyman-engine-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_ENGINE/keyman32.pdb" //t keyman-engine-windows
}

function do_install() {
  cp "$WINDOWS_PROGRAM_ENGINE/keyman32.dll" "$INSTALLPATH_KEYMANENGINE/keyman32.dll"
  echo "You may want to manually tweak keyman-debug-etw.man and fill in $INSTALLPATH_KEYMANENGINE/keyman32.dll"
  echo "and then run wevtutil im keyman-debug-etw.man to get the latest event tracing"
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action publish:project      do_publish
builder_run_action install:project      do_install
