#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Test the read of basic keyboard info info from js files" clean configure build test
builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/KeyboardJSInfoTestSuite.exe"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /developer/src/test/auto/keyboard-js-info/$WIN32_TARGET

#-------------------------------------------------------------------------------------------------------------------

builder_run_action clean:project        rm -rf bin obj manifest.res manifest.xml *.dproj.local version.res icons.RES icons.res *.identcache
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        delphi_msbuild KeyboardJSInfoTestSuite.dproj "//p:Platform=Win32" "//p:CI=CI"
builder_run_action test:project         "$WIN32_TARGET" -b -exit:continue

# NOTE: The .dproj needs $(CI) added to the Delphi Compiler/Conditional defines (All
# configurations - all platforms) section in order for the CI flag to be passed in.
# (It's best to make this change in Delphi IDE).
