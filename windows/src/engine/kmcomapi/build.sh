#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "COM API library" \
  @/common/include \
  @/common/windows/delphi \
  clean configure build test publish install edit

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/kmcomapi.dll"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /windows/src/engine/kmcomapi/$WIN32_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  clean_windows_project_files
  rm -f kbd_noicon.res kmcomapi.tlb
}

function do_build() {
  create-windows-output-folders
  build_version.res
  build_manifest.res
  run_in_vs_env rc kbd_noicon.rc
  run_in_vs_env gentlb -Tkmcomapi.tlb kmcomapi.ridl

  delphi_msbuild kmcomapi.dproj "//p:Platform=Win32"
  sentrytool_delphiprep "$WIN32_TARGET" kmcomapi.dpr
  tds2dbg "$WIN32_TARGET"

  delphi_msbuild kmcomapi.dproj "//p:Platform=Win64"
  # Delphi does not allow us to build to a different target filename so we rename after build
  mv -f "$WIN64_TARGET_PATH/kmcomapi.dll" "$WIN64_TARGET_PATH/kmcomapi.x64.dll"

  cp "$WIN32_TARGET" "$WINDOWS_PROGRAM_ENGINE"
  cp "$WIN32_TARGET_PATH/kmcomapi.dbg" "$WINDOWS_DEBUGPATH_ENGINE/kmcomapi.dbg"
  cp "$WIN64_TARGET_PATH/kmcomapi.x64.dll" "$WINDOWS_PROGRAM_ENGINE/kmcomapi.x64.dll"

  # x64 Delphi symbols not supported: cp "$WIN64_TARGET_PATH/kmcomapi.dbg" "$WINDOWS_PROGRAM_ENGINE/kmcomapi.x64.dbg"
}

function do_publish() {
  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/kmcomapi.dll"
  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/kmcomapi.x64.dll"

  wrap-symstore "$WINDOWS_PROGRAM_ENGINE/kmcomapi.dll" //t keyman-engine-windows
  wrap-symstore "$WINDOWS_PROGRAM_ENGINE/kmcomapi.x64.dll" //t keyman-engine-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_ENGINE/kmcomapi.dbg" //t keyman-engine-windows
  # No support for 64-bit delphi symbols
}

function do_install() {
  regsvr32 //s //u "$INSTALLPATH_KEYMANENGINE/kmcomapi.dll"
  cp "$WINDOWS_PROGRAM_ENGINE/kmcomapi.dll" "$INSTALLPATH_KEYMANENGINE/kmcomapi.dll"
  regsvr32 //s "$INSTALLPATH_KEYMANENGINE/kmcomapi.dll"

  regsvr32 //s //u "$INSTALLPATH_KEYMANENGINE/kmcomapi.x64.dll"
  cp "$WINDOWS_PROGRAM_ENGINE/kmcomapi.x64.dll" "$INSTALLPATH_KEYMANENGINE/kmcomapi.x64.dll"
  regsvr32 //s "$INSTALLPATH_KEYMANENGINE/kmcomapi.x64.dll"
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action publish:project      do_publish
builder_run_action install:project      do_install
builder_run_action edit:project         start kmcompai.dproj

