#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Build Keyman Developer Setup" clean configure build test
builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/setup.exe"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /developer/src/setup/$WIN32_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  rm -rf bin obj manifest.res manifest.xml *.dproj.local version.res icons.RES icons.res *.identcache
}

function do_build() {
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
  # SIGNCODE /d "Keyman Developer Setup" $(DEVELOPER_PROGRAM)\setup.exe
}

# TODO
function do_publish() {
  "$SYMSTORE" "$DEVELOPER_PROGRAM"/setup.exe //t keyman-developer
  "$SYMSTORE" "$DEVELOPER_ROOT"/src/setup/setup.dbg //t keyman-developer
}

# TODO
function do_pre_release_build() {
  # test that (a) linked manifest exists and correct, and (b) has uiAccess=true
  "$MT" -nologo -inputresource:"$DEVELOPER_PROGRAM"/setup.exe -validate_manifest
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
