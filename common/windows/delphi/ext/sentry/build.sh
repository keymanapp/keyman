#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Sentry component for Delphi" clean configure build test
builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh

#-------------------------------------------------------------------------------------------------------------------

function do_test() {
  # TODO: this does not currently seem to be tested or used, and no longer
  #       builds but this can be fixed in the future
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

# builder_run_action clean:project        # nothing to clean
builder_run_action configure:project    configure_windows_build_environment
# builder_run_action build:project        # nothing to build
# builder_run_action test:project         do_test
