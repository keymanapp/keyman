#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Build Keyman Developer Setup" \
  @/common/windows/delphi \
  clean configure build test publish

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/setup.exe"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /developer/src/setup/$WIN32_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_build() {
  create-developer-output-folders
  run_in_vs_env rc icons.rc
  build_version.res
  build_manifest.res
  delphi_msbuild setup.dproj "//p:Platform=Win32"
  sentrytool_delphiprep "$WIN32_TARGET" setup.dpr
  tds2dbg "$WIN32_TARGET"
  cp "$WIN32_TARGET" "$DEVELOPER_PROGRAM"
  if [[ -f "$WIN32_TARGET_PATH/setup.dbg" ]]; then
    cp "$WIN32_TARGET_PATH/setup.dbg" "$DEVELOPER_DEBUGPATH/devsetup.dbg"
  fi
  rm -f "$WIN32_TARGET_PATH/devsetup.dbg"
  if [[ -f "$WIN32_TARGET_PATH/setup.dbg" ]]; then
    mv "$WIN32_TARGET_PATH/setup.dbg" "$WIN32_TARGET_PATH/devsetup.dbg"
  fi
}

function do_publish() {
  wrap-mt -nologo -inputresource:"$DEVELOPER_PROGRAM"/setup.exe -validate_manifest
  # Not signing here because we need to sign the bundled installer instead
  # wrap-signcode /d "Keyman Developer Setup" $(DEVELOPER_PROGRAM)\setup.exe
  wrap-symstore "$DEVELOPER_PROGRAM/setup.exe" //t keyman-developer
  wrap-symstore "$DEVELOPER_DEBUGPATH/setup.dbg" //t keyman-developer
}

builder_run_action clean:project        clean_windows_project_files
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
builder_run_action publish:project      do_publish
# builder_run_action test:project         do_test
