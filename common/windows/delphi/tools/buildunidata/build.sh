#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Unicode character database build tool" \
  @/common/windows/delphi:keymanversion \
  clean configure build test edit
builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /common/windows/delphi/tools/buildunidata/$WIN32_TARGET_PATH/buildunidata.exe

#-------------------------------------------------------------------------------------------------------------------

function do_build() {
  delphi_msbuild buildunidata.dproj "//p:Platform=Win32"
  # The data transform step is done in common/windows/data/build.sh
  # "$WIN32_TARGET_PATH/buildunidata.exe" "$KEYMAN_ROOT/common/windows/data" "$KEYMAN_ROOT/common/windows/bin/data/unicodedata.mdb"
}

builder_run_action clean:project        clean_windows_project_files
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action edit:project         start buildunidata.dproj
