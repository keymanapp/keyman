#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Build imsample" clean configure build test publish
builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/imsample.dll"
X64_TARGET="$X64_TARGET_PATH/imsample.x64.dll"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /developer/src/samples/imsample/$X64_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  vs_msbuild imsample.sln //t:Clean
  rm -rf bin obj manifest.res manifest.xml version.res
}

function do_build() {
  build_version.res
  vs_msbuild imsample.sln //t:Build "//p:Platform=Win32"
  cp "$WIN32_TARGET" "$DEVELOPER_PROGRAM"
  cp "$WIN32_TARGET_PATH/imsample.pdb" "$DEVELOPER_DEBUGPATH"

  vs_msbuild imsample.sln //t:Build "//p:Platform=x64"
  cp "$X64_TARGET" "$DEVELOPER_PROGRAM"
  cp "$X64_TARGET_PATH/imsample.x64.pdb" "$DEVELOPER_DEBUGPATH"
}

# TODO
function do_publish() {
  wrap-signcode //d "Keyman IMX Sample" "$DEVELOPER_PROGRAM/imsample.dll"
  wrap-signcode //d "Keyman IMX Sample" "$DEVELOPER_PROGRAM/imsample.x64.dll"
  wrap-symstore "$DEVELOPER_PROGRAM/imsample.dll" //t keyman-developer
  wrap-symstore "$DEVELOPER_PROGRAM/imsample.x64.dll" //t keyman-developer
  wrap-symstore "$DEVELOPER_DEBUGPATH/imsample.pdb" //t keyman-developer
  wrap-symstore "$DEVELOPER_DEBUGPATH/imsample.x64.pdb" //t keyman-developer
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action publish:project      do_publish
