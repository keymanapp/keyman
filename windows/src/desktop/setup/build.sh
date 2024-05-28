#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Keyman Setup bootstrap" \
  @/common/include \
  @/common/windows/delphi \
  clean configure build test publish debug-manifest

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/setup.exe"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /windows/src/desktop/setup/$WIN32_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  clean_windows_project_files
  rm -f Keyman.Setup.System.Locale*.pas
  rm -f icons.res
}

function do_build() {
  create-windows-output-folders
  build_version.res
  build_manifest.res
  run_in_vs_env rc icons.rc
  "$DEVTOOLS" -buildsetupstrings locale\\ .\\

  delphi_msbuild setup.dproj "//p:Platform=Win32"
  sentrytool_delphiprep "$WIN32_TARGET" setup.dpr
  tds2dbg "$WIN32_TARGET"

  cp "$WIN32_TARGET" "$WINDOWS_PROGRAM_APP"
  # setup-redist.exe does not get signed as it is intended for a bundled installer
  cp "$WIN32_TARGET" "$WINDOWS_PROGRAM_APP/setup-redist.exe"
  cp "$WIN32_TARGET_PATH/setup.dbg" "$WINDOWS_DEBUGPATH_APP/setup.dbg"
}

function do_build_debug_manifest() {
  # make a non-elevated manifest.res for testing purposes
  rm -f manifest.res
  cp manifest.in std-manifest.tmp
  cp debug-manifest.in manifest.in
  build_manifest.res
  mv -f std-manifest.tmp manifest.in
}

function do_publish() {
  # test that (a) linked manifest exists and correct
  wrap-mt -nologo -inputresource:"$WINDOWS_PROGRAM_APP/setup.exe" -validate_manifest

  wrap-signcode //d "Keyman for Windows Setup" "$WINDOWS_PROGRAM_APP/setup.exe"
  wrap-symstore "$WINDOWS_PROGRAM_APP/setup.exe" //t keyman-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_APP/setup.dbg" //t keyman-windows
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action publish:project      do_publish

builder_run_action debug-manifest:project  do_build_debug_manifest
