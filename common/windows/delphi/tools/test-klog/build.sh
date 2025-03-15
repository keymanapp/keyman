#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Tool for validating klog is disabled for release builds" \
  clean configure build test prepublish edit
builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /common/windows/delphi/tools/test-klog/$WIN32_TARGET_PATH/test_klog.exe

builder_describe_internal_dependency \
  prepublish:project   build:project

#-------------------------------------------------------------------------------------------------------------------

function do_build() {
  # verify that the klog module is disabled for release builds,
  # so we always clean and rebuild
  clean_windows_project_files
  delphi_msbuild test_klog.dproj "//p:Platform=Win32"
}

builder_run_action clean:project        clean_windows_project_files
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action prepublish:project  "$WIN32_TARGET_PATH/test_klog.exe"
builder_run_action edit:project         start test_klog.dproj
