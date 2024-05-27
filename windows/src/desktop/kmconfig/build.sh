#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Keyman low-level configuration tool" \
  @/common/include \
  @/common/windows/delphi \
  clean configure build test publish install

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/kmconfig.exe"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /windows/src/desktop/kmconfig/$WIN32_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_build() {
  create-windows-output-folders
  build_version.res
  build_manifest.res
  delphi_msbuild kmconfig.dproj "//p:Platform=Win32"
  sentrytool_delphiprep "$WIN32_TARGET" kmconfig.dpr
  tds2dbg "$WIN32_TARGET"

  cp "$WIN32_TARGET" "$WINDOWS_PROGRAM_APP"
  cp "$WIN32_TARGET_PATH/kmconfig.dbg" "$WINDOWS_DEBUGPATH_APP/kmconfig.dbg"
}

function do_publish() {
  # test that (a) linked manifest exists and correct
  wrap-mt -nologo -inputresource:"$WINDOWS_PROGRAM_APP/kmconfig.exe" -validate_manifest

  wrap-signcode //d "Keyman for Windows" "$WINDOWS_PROGRAM_APP/kmconfig.exe"
  wrap-symstore "$WINDOWS_PROGRAM_APP/kmconfig.exe" //t keyman-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_APP/kmconfig.dbg" //t keyman-windows
}

builder_run_action clean:project        clean_windows_project_files
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action publish:project      do_publish
builder_run_action install:project      cp "$WINDOWS_PROGRAM_APP/kmconfig.exe" "$INSTALLPATH_KEYMANAPP/kmconfig.exe"
