#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# TODO: move this out of ext and into tools

builder_describe "tds2dbg component" clean configure build test
builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        "$WIN32_TARGET_PATH/tds2dbg.exe"

#-------------------------------------------------------------------------------------------------------------------

function do_build() {
  mkdir -p "$WIN32_TARGET_PATH"
  cp tds2dbg.bin "$WIN32_TARGET_PATH/tds2dbg.exe"

  mkdir -p "$KEYMAN_ROOT/common/windows/bin/tools"
  cp "$WIN32_TARGET_PATH/tds2dbg.exe" "$KEYMAN_ROOT/common/windows/bin/tools/"
}

builder_run_action clean:project        clean_windows_project_files
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
