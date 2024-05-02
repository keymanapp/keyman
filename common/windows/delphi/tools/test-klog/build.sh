#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Build test-klog tool" clean configure build test verify
builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /common/windows/delphi/tools/sentrytool/$WIN32_TARGET_PATH/sentrytool.exe

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  rm -rf obj manifest.res manifest.xml *.dproj.local version.res icons.RES icons.res *.identcache
}

function do_verify() {
  # verify that the klog module is disabled for release builds
  do_clean
  delphi_msbuild test_klog.dproj "//p:Platform=Win32"
  "$WIN32_TARGET_PATH/test_klog.exe"
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
# builder_run_action build:project        do_build
# builder_run_action test:project         do_test

# Kept separate from test as this is not so much a unit test as an environment
# safety check
builder_run_action verify:project       do_verify
