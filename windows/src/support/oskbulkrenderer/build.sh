#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "oskbuildrenderer" \
  clean configure build test edit

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/oskbulkrenderer.exe"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /windows/src/support/oskbulkrenderer/$WIN32_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  clean_windows_project_files
}

function do_build() {
  create-windows-output-folders
  build_version.res
  delphi_msbuild oskbulkrenderer.dproj "//p:Platform=Win32"

  cp "$WIN32_TARGET" "$WINDOWS_PROGRAM_SUPPORT"
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action edit:project         start oskbulkrenderer.dproj
