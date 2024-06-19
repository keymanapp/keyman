#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Keystroke processing engine (64 bit)" \
  @/common/include \
  @/core:x64 \
  clean configure build test publish install

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
X64_TARGET="$X64_TARGET_PATH/keyman64.dll"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /windows/src/engine/keyman64/$X64_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  vs_msbuild keyman64.vcxproj //t:Clean
  clean_windows_project_files
}

function do_build() {
  create-windows-output-folders
  build_version.res
  vs_msbuild keyman64.vcxproj //t:Build "//p:Platform=x64"
  cp "$X64_TARGET" "$WINDOWS_PROGRAM_ENGINE"
  cp "$X64_TARGET_PATH/keyman64.pdb" "$WINDOWS_DEBUGPATH_ENGINE"
}

function do_publish() {
  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/keyman64.dll"
  wrap-symstore "$WINDOWS_PROGRAM_ENGINE/keyman64.dll" //t keyman-engine-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_ENGINE/keyman64.pdb" //t keyman-engine-windows
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action publish:project      do_publish
builder_run_action install:project      cp "$WINDOWS_PROGRAM_ENGINE/keyman64.dll" "$INSTALLPATH_KEYMANENGINE/keyman64.dll"
