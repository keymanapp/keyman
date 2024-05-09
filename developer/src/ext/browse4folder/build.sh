#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Build browser4folder component" clean configure build test
builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /developer/lib/browse4folder.bpl

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  rm -rf obj manifest.res manifest.xml *.dproj.local version.res icons.RES icons.res *.identcache
}

function do_build() {
  mkdir -p obj/Win32/$TARGET_PATH
  pwd
  delphi_msbuild browse4folder.dproj "//p:Platform=Win32"
  "$DEVTOOLS" -ip "$DEVELOPER_OUTLIB/browse4folder.bpl"
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
