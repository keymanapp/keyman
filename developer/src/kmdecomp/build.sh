#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
  "Unsupported .kmx decompiler" \
  clean configure build test publish install

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/kmdecomp.exe"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /developer/src/kmdecomp/$WIN32_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  vs_msbuild kmdecomp.sln //t:Clean
  rm -rf bin obj manifest.res manifest.xml version.res
}

function do_build() {
  create-developer-output-folders
  build_version.res
  vs_msbuild kmdecomp.sln //t:Build "//p:Platform=Win32"
  cp "$WIN32_TARGET" "$DEVELOPER_PROGRAM"
  cp "$WIN32_TARGET_PATH/kmdecomp.pdb" "$DEVELOPER_DEBUGPATH"
}

function do_publish() {
  wrap-signcode //d "Keyman Developer Decompiler" "$DEVELOPER_PROGRAM/kmdecomp.exe"
  wrap-symstore "$DEVELOPER_PROGRAM/kmdecomp.exe" //t keyman-developer
  wrap-symstore "$DEVELOPER_DEBUGPATH/kmdecomp.pdb" //t keyman-developer
}

function do_install() {
  cp "$DEVELOPER_PROGRAM/kmdecomp.exe" "$INSTALLPATH_KEYMANDEVELOPER/kmdecomp.exe"
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action publish:project      do_publish
builder_run_action install:project      do_install
