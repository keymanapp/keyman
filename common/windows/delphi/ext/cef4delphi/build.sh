#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "cef4delphi component" clean configure build test
builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /common/windows/lib/CEF4Delphi.bpl

#-------------------------------------------------------------------------------------------------------------------

function do_build() {
  "$DEVTOOLS" -ai "$THIS_SCRIPT_PATH/source"
  delphi_msbuild packages/CEF4Delphi.dproj "//p:Platform=Win32"
  "$DEVTOOLS" -ip "$COMMON_OUTLIB/CEF4Delphi.bpl"
}

builder_run_action clean:project        clean_windows_project_files
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
