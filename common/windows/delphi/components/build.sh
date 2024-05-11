#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Common Delphi components" clean configure build test
builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /common/windows/lib/common_components.bpl

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  rm -rf obj manifest.res manifest.xml *.dproj.local version.res icons.RES icons.res *.identcache
}

function do_build() {
  mkdir -p obj/Win32/$TARGET_PATH
  delphi_msbuild common_components.dproj "//p:Platform=Win32"
  "$DEVTOOLS" -ip "$COMMON_OUTLIB/common_components.bpl"
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
