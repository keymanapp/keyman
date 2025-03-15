#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Keyman Configuration" \
  @/common/include \
  @/common/windows/delphi \
  @/windows/src/global/delphi:message-identifiers \
  clean configure build test publish install edit

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/kmshell.exe"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /windows/src/desktop/kmshell/$WIN32_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  clean_windows_project_files
  rm -f icons.res
}

function do_build() {
  create-windows-output-folders
  build_version.res
  build_manifest.res
  run_in_vs_env rc icons.rc
  delphi_msbuild kmshell.dproj "//p:Platform=Win32"
  sentrytool_delphiprep "$WIN32_TARGET" kmshell.dpr
  tds2dbg "$WIN32_TARGET"

  cp "$WIN32_TARGET" "$WINDOWS_PROGRAM_APP"
  cp "$WIN32_TARGET_PATH/kmshell.dbg" "$WINDOWS_DEBUGPATH_APP/kmshell.dbg"

  do_build_data
}

function do_build_data() {
  replaceVersionStrings_Mkver xml/sentry.init.js.in xml/sentry.init.js

  rm -rf "$WINDOWS_PROGRAM_APP/xml"
  mkdir -p "$WINDOWS_PROGRAM_APP/xml"
  cp -r xml/* "$WINDOWS_PROGRAM_APP/xml/"

  rm -rf "$WINDOWS_PROGRAM_APP/locale"
  mkdir -p "$WINDOWS_PROGRAM_APP/locale"
  cp -r locale/* "$WINDOWS_PROGRAM_APP/locale/"
}

function do_publish() {
  # test that (a) linked manifest exists and correct
  wrap-mt -nologo -inputresource:"$WINDOWS_PROGRAM_APP/kmshell.exe" -validate_manifest

  wrap-signcode //d "Keyman for Windows" "$WINDOWS_PROGRAM_APP/kmshell.exe"
  wrap-symstore "$WINDOWS_PROGRAM_APP/kmshell.exe" //t keyman-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_APP/kmshell.dbg" //t keyman-windows
}

function do_install() {
  cp "$WINDOWS_PROGRAM_APP/kmshell.exe" "$INSTALLPATH_KEYMANAPP/kmshell.exe"
  rm -rf "$INSTALLPATH_KEYMANAPP/xml"
  mkdir -p "$INSTALLPATH_KEYMANAPP/xml"
  cp -r "$WINDOWS_PROGRAM_APP/xml"/* "$INSTALLPATH_KEYMANAPP/xml/"

  rm -rf "$INSTALLPATH_KEYMANAPP/locale"
  mkdir -p "$INSTALLPATH_KEYMANAPP/locale"
  cp -r "$WINDOWS_PROGRAM_APP/locale"/* "$INSTALLPATH_KEYMANAPP/locale/"
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action publish:project      do_publish
builder_run_action install:project      do_install
builder_run_action edit:project         start kmshell.dproj
