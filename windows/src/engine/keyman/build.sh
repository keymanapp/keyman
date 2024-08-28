#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Keyman main host process (32-bit)" \
  @/common/include \
  @/common/windows/delphi \
  @/windows/src/global/delphi \
  clean configure build test publish install debug-manifest

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/keyman.exe"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /windows/src/engine/keyman/$WIN32_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  clean_windows_project_files
  rm -f keymanmenuitem.res icons.res osktoolbar.res langswitch/langswitchmanager.res
}

function do_build() {
  create-windows-output-folders
  build_version.res
  build_manifest.res
  run_in_vs_env rc keymanmenuitem.rc
  run_in_vs_env rc icons.rc
  run_in_vs_env rc osktoolbar.rc
  run_in_vs_env rc langswitch/langswitchmanager.rc
  delphi_msbuild keyman.dproj "//p:Platform=Win32"
  sentrytool_delphiprep "$WIN32_TARGET" keyman.dpr
  tds2dbg "$WIN32_TARGET"

  cp "$WIN32_TARGET" "$WINDOWS_PROGRAM_ENGINE"
  cp "$WIN32_TARGET_PATH/keyman.dbg" "$WINDOWS_DEBUGPATH_ENGINE/keyman.dbg"

  # Also copy sentry files here
  cp "$KEYMAN_ROOT/common/windows/delphi/ext/sentry/sentry.dll" \
     "$KEYMAN_ROOT/common/windows/delphi/ext/sentry/sentry.x64.dll" \
     "$KEYMAN_ROOT/common/windows/delphi/ext/sentry/crashpad_handler.exe" \
     "$KEYMAN_ROOT/windows/bin/engine/"
}

function do_publish() {
  # test that (a) linked manifest exists and correct
  wrap-mt -nologo -inputresource:"$WINDOWS_PROGRAM_ENGINE/keyman.exe" -validate_manifest
  wrap-mt -nologo -inputresource:"$WINDOWS_PROGRAM_ENGINE/keyman.exe" -out:temp.manifest
  grep -q 'uiAccess="true"' temp.manifest || builder_die 'uiAccess must be set to true in manifest.xml'
  rm -f temp.manifest

  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/keyman.exe"
  # Also sign sentry files here
  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/sentry.dll"
  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/sentry.x64.dll"
  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/crashpad_handler.exe"

  wrap-symstore "$WINDOWS_PROGRAM_ENGINE/keyman.exe" //t keyman-engine-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_ENGINE/keyman.dbg" //t keyman-engine-windows
}

function do_build_debug_manifest() {
  # make a non-elevated manifest.res for testing purposes
  rm -f manifest.res
  cp manifest.in std-manifest.tmp
  cp debug-manifest.in manifest.in
  build_manifest.res
  mv -f std-manifest.tmp manifest.in
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action publish:project      do_publish
builder_run_action install:project      cp "$WINDOWS_PROGRAM_ENGINE/keyman.exe" "$INSTALLPATH_KEYMANENGINE/keyman.exe"

builder_run_action debug-manifest:project  do_build_debug_manifest
