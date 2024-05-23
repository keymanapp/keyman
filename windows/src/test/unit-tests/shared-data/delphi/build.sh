#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "shared data Delphi" clean configure build
builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/delphishareddata.exe"
WIN64_TARGET="$WIN64_TARGET_PATH/delphishareddata.exe"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /windows/src/test/unit-tests/shared-data/delphi/$WIN32_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_build() {
  delphi_msbuild delphishareddata.dproj "//p:Platform=Win32"
  delphi_msbuild delphishareddata.dproj "//p:Platform=Win64"
}

builder_run_action clean:project        clean_windows_project_files
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build

# NOTE: The .dproj needs $(CI) added to the Delphi Compiler/Conditional defines (All
# configurations - all platforms) section in order for the CI flag to be passed in.
# (It's best to make this change in Delphi IDE).
