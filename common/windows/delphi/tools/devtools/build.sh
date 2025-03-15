#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
  "Development and build utilities for Delphi" \
  @/common/windows/delphi:keymanversion \
  clean configure build test prepublish edit

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /common/windows/delphi/tools/devtools/$WIN32_TARGET_PATH/devtools.exe

builder_describe_internal_dependency \
  prepublish:project   build:project

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean:project        clean_windows_project_files
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        delphi_msbuild devtools.dproj "//p:Platform=Win32"
# builder_run_action test:project         do_test
builder_run_action prepublish:project   "$DEVTOOLS" -rt
builder_run_action edit:project         start devtools.dproj
