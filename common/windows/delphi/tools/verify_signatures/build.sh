#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Tool to verify all Windows executable signatures and manifests" \
  clean configure build test verify edit
builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /common/windows/delphi/tools/verify_signatures/$WIN32_TARGET_PATH/verify_signatures.exe

#-------------------------------------------------------------------------------------------------------------------

function do_build() {
  delphi_msbuild verify_signatures.dproj "//p:Platform=Win32"
  cp sigcheck.bin "$WIN32_TARGET_PATH/sigcheck.exe"
}

builder_run_action clean:project        clean_windows_project_files
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action edit:project         start verify_signatures.dproj
