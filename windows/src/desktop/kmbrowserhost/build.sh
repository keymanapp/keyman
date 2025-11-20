#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Chromium browser host process for Keyman" \
  @/common/include \
  @/common/windows/delphi \
  clean configure build test publish install edit

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/kmbrowserhost.exe"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /windows/src/desktop/kmbrowserhost/$WIN32_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_build() {
  create-windows-output-folders
  build_version.res
  build_manifest.res
  delphi_msbuild kmbrowserhost.dproj "//p:Platform=Win32"
  do_map2pdb "$WIN32_TARGET_PATH/kmbrowserhost.map" "$WIN32_TARGET"

  cp "$WIN32_TARGET" "$WINDOWS_PROGRAM_APP"
  cp_if_exists "$WIN32_TARGET_PATH/kmbrowserhost.pdb" "$WINDOWS_DEBUGPATH_APP"
}

function do_publish() {
  # test that (a) linked manifest exists and correct
  wrap-mt -nologo -inputresource:"$WINDOWS_PROGRAM_APP/kmbrowserhost.exe" -validate_manifest

  wrap-signcode //d "Keyman for Windows" "$WINDOWS_PROGRAM_APP/kmbrowserhost.exe"
  wrap-symstore "$WINDOWS_PROGRAM_APP/kmbrowserhost.exe" //t keyman-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_APP/kmbrowserhost.pdb" //t keyman-windows
}

function do_install() {
  cp "$WINDOWS_PROGRAM_APP/kmbrowserhost.exe" "$INSTALLPATH_KEYMANAPP/kmbrowserhost.exe"
  cp_if_exists "$WINDOWS_DEBUGPATH_APP/kmbrowserhost.pdb" "$INSTALLPATH_KEYMANAPP/kmbrowserhost.pdb"
}

builder_run_action clean:project        clean_windows_project_files
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action publish:project      do_publish
builder_run_action install:project      do_install
builder_run_action edit:project         start kmbrowserhost.dproj
