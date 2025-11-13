#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Keyboard project generation and conversion tool" \
  @/common/include \
  @/common/windows/delphi \
  clean configure build test publish install edit

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/kmconvert.exe"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /developer/src/kmconvert/$WIN32_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_build() {
  create-developer-output-folders
  build_version.res
  build_manifest.res
  run_in_vs_env rc icons.rc
  delphi_msbuild kmconvert.dproj "//p:Platform=Win32"
  do_map2pdb "$WIN32_TARGET_PATH/kmconvert.map" "$WIN32_TARGET"

  cp "$WIN32_TARGET" "$DEVELOPER_PROGRAM"
  cp_if_exists "$WIN32_TARGET_PATH/kmconvert.pdb" "$DEVELOPER_DEBUGPATH"

  rm -rf "$DEVELOPER_PROGRAM/projects/templates"
  mkdir -p "$DEVELOPER_PROGRAM/projects/templates"
  cp -R data/* "$DEVELOPER_PROGRAM/projects/templates"
}

function do_test() {
  cd test
  delphi_msbuild kmconverttest.dproj "//p:Platform=Win32" "//p:CI=CI"
  $WIN32_TARGET_PATH/kmconverttest.exe -b -exit:continue
  cd ..
}

function do_publish() {
  # test that (a) linked manifest exists and correct
  wrap-mt -nologo -inputresource:"$DEVELOPER_PROGRAM/kmconvert.exe" -validate_manifest

  wrap-signcode //d "Keyman Developer" "$DEVELOPER_PROGRAM/kmconvert.exe"
  wrap-symstore "$DEVELOPER_PROGRAM/kmconvert.exe" //t keyman-developer
  wrap-symstore "$DEVELOPER_DEBUGPATH/kmconvert.pdb" //t keyman-developer
}

function do_install() {
  cp "$DEVELOPER_PROGRAM/kmconvert.exe" "$INSTALLPATH_KEYMANDEVELOPER/kmconvert.exe"
  cp_if_exists "$DEVELOPER_DEBUGPATH/kmconvert.pdb" "$INSTALLPATH_KEYMANDEVELOPER/kmconvert.pdb"
}

builder_run_action clean:project        clean_windows_project_files
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
builder_run_action test:project         do_test
builder_run_action publish:project      do_publish
builder_run_action install:project      do_install
builder_run_action edit:project         start kmconvert.dproj
