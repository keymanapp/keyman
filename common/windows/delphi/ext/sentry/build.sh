#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Build sentry component" clean configure build test
builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh
  # build:project        /common/windows/lib/CEF4Delphi.bpl

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  rm -rf obj manifest.res manifest.xml *.dproj.local version.res icons.RES icons.res *.identcache
}

function do_build() {
  "$DEVTOOLS" -ai "$THIS_SCRIPT_PATH/source"
  delphi_msbuild packages/CEF4Delphi.dproj "//p:Platform=Win32"
  "$DEVTOOLS" -ip "$OUTLIB/CEF4Delphi.bpl"
}

function do_test() {
  # TODO: this does not currently seem to be tested or used
  cd test
  delphi_msbuild SentryClientTest.dproj "//p:Platform=Win32"
  delphi_msbuild SentryClientVclTest.dproj "//p:Platform=Win32"

  sentrytool_delphiprep "$WIN32_TARGET_PATH/SentryClientTest.exe" -dpr SentryClientTest.dpr
  sentrytool_delphiprep "$WIN32_TARGET_PATH/SentryClientVclTest.exe" -dpr SentryClientVclTest.dpr
  tds2dbg "$WIN32_TARGET_PATH/SentryClientTest.exe"
  tds2dbg "$WIN32_TARGET_PATH/SentryClientVclTest.exe"

  sentry-cli upload-dif -p keyman-windows --wait --include-sources .

  cd ..
}

# builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
# builder_run_action build:project        do_build
# builder_run_action test:project         do_test
