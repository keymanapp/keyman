#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Sentrytool for converting Delphi symbols into sentry-readable format" clean configure build test
builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /common/windows/delphi/tools/sentrytool/$WIN32_TARGET_PATH/sentrytool.exe

#-------------------------------------------------------------------------------------------------------------------

function do_test() {
  # TODO: this test does not run, fix it later
  cd Test
  delphi_msbuild sentrytoolTests.dpr "//p:Platform=Win32" "//p:CI=CI"
  ./$WIN32_TARGET_PATH/sentrytoolTests.exe -b -exit:continue
  cd ..
}

builder_run_action clean:project        clean_windows_project_files
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        delphi_msbuild sentrytool.dproj "//p:Platform=Win32"
# builder_run_action test:project         do_test
