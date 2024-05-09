#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Build build_standards_data tool" clean configure build test
builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /common/windows/delphi/standards/Keyman.System.Standards.LangTagsRegistry.pas

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  rm -rf obj manifest.res manifest.xml *.dproj.local version.res icons.RES icons.res *.identcache
}

STANDARDS_DATA_PATH="$KEYMAN_ROOT/resources/standards-data"
STANDARDS_OUT_PATH="$KEYMAN_ROOT/common/windows/delphi/standards"

ISO6393_DATA="$STANDARDS_DATA_PATH/iso639-3/iso639-3.tab"
ISO6393_PAS="$STANDARDS_OUT_PATH/Keyman.System.Standards.ISO6393ToBCP47Registry.pas"

SUBTAG_DATA="$STANDARDS_DATA_PATH/language-subtag-registry/language-subtag-registry"
SUBTAG_PAS="$STANDARDS_OUT_PATH/Keyman.System.Standards.BCP47SubtagRegistry.pas"

SUPPRESS_DATA="$STANDARDS_DATA_PATH/language-subtag-registry/language-subtag-registry"
SUPPRESS_PAS="$STANDARDS_OUT_PATH/Keyman.System.Standards.BCP47SuppressScriptRegistry.pas"

LCID_DATA="$STANDARDS_DATA_PATH/windows-lcid-to-bcp-47/map_clean_win.txt"
LCID_PAS="$STANDARDS_OUT_PATH/Keyman.System.Standards.LCIDToBCP47Registry.pas"

LANGTAGS_DATA="$STANDARDS_DATA_PATH/langtags/langtags.json"
LANGTAGS_PAS="$STANDARDS_OUT_PATH/Keyman.System.Standards.LangTagsRegistry.pas"

function do_build() {
  delphi_msbuild build_standards_data.dproj "//p:Platform=Win32"
  "$WIN32_TARGET_PATH/build_standards_data.exe" iso6393  "$ISO6393_DATA"  "$ISO6393_PAS"
  "$WIN32_TARGET_PATH/build_standards_data.exe" suppress "$SUPPRESS_DATA" "$SUPPRESS_PAS"
  "$WIN32_TARGET_PATH/build_standards_data.exe" subtag   "$SUBTAG_DATA"   "$SUBTAG_PAS"
  "$WIN32_TARGET_PATH/build_standards_data.exe" lcid     "$LCID_DATA"     "$LCID_PAS"
  "$WIN32_TARGET_PATH/build_standards_data.exe" langtags "$LANGTAGS_DATA" "$LANGTAGS_PAS"
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
