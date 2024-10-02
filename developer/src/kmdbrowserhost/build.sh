#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Chromium browser host process for Keyman Developer" \
  @/common/include \
  @/common/windows/delphi \
  clean configure build test publish install

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/kmdbrowserhost.exe"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /developer/src/kmdbrowserhost/$WIN32_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_build() {
  create-developer-output-folders
  build_version.res
  build_manifest.res
  delphi_msbuild kmdbrowserhost.dproj "//p:Platform=Win32"
  sentrytool_delphiprep "$WIN32_TARGET" kmdbrowserhost.dpr
  tds2dbg "$WIN32_TARGET"

  cp "$WIN32_TARGET" "$DEVELOPER_PROGRAM"
  if [[ -f "$WIN32_TARGET_PATH/kmdbrowserhost.dbg" ]]; then
    cp "$WIN32_TARGET_PATH/kmdbrowserhost.dbg" "$DEVELOPER_DEBUGPATH"
  fi
}

function do_publish() {
  # test that (a) linked manifest exists and correct
  wrap-mt -nologo -inputresource:"$DEVELOPER_PROGRAM/kmdbrowserhost.exe" -validate_manifest

  wrap-signcode //d "Keyman Developer" "$DEVELOPER_PROGRAM/kmdbrowserhost.exe"
  wrap-symstore "$DEVELOPER_PROGRAM/kmdbrowserhost.exe" //t keyman-developer
  wrap-symstore "$DEVELOPER_DEBUGPATH/kmdbrowserhost.dbg" //t keyman-developer
}

builder_run_action clean:project        clean_windows_project_files
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action publish:project      do_publish
builder_run_action install:project      cp "$DEVELOPER_PROGRAM/kmdbrowserhost.exe" "$INSTALLPATH_KEYMANDEVELOPER/kmdbrowserhost.exe"
