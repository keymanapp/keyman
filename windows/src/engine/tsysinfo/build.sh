#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Diagnostics (32-bit)" \
  @/common/include \
  @/common/windows/delphi \
  @/windows/src/engine/tsysinfox64 \
  clean configure build test publish install

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/tsysinfo.exe"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /windows/src/engine/tsysinfo/$WIN32_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  clean_windows_project_files
  rm -f tsysinfox64.bin tsysinfo_x64.res
}

function do_build() {
  create-windows-output-folders
  build_version.res
  build_manifest.res

  # Embed tsysinfox64 into a resource; we have to do a special signcode for
  # tsysinfox64.exe as we embed the executable into tsysinfo.exe
  ../tsysinfox64/build.sh publish --no-deps
  cp "$WINDOWS_PROGRAM_ENGINE/tsysinfox64.exe" tsysinfox64.bin
  run_in_vs_env rc tsysinfo_x64.rc
  rm -f tsysinfox64.bin

  delphi_msbuild tsysinfo.dproj "//p:Platform=Win32"
  sentrytool_delphiprep "$WIN32_TARGET" tsysinfo.dpr
  tds2dbg "$WIN32_TARGET"

  cp "$WIN32_TARGET" "$WINDOWS_PROGRAM_ENGINE"
  cp *.xslt "$WINDOWS_PROGRAM_ENGINE/"
  cp "$WIN32_TARGET_PATH/tsysinfo.dbg" "$WINDOWS_DEBUGPATH_ENGINE/tsysinfo.dbg"
}

function do_publish() {
  # test that (a) linked manifest exists and correct
  wrap-mt -nologo -inputresource:"$WINDOWS_PROGRAM_ENGINE/keyman.exe" -validate_manifest

  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/tsysinfo.exe"

  wrap-symstore "$WINDOWS_PROGRAM_ENGINE/tsysinfo.exe" //t keyman-engine-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_ENGINE/tsysinfo.dbg" //t keyman-engine-windows
}

function do_install() {
  cp "$WINDOWS_PROGRAM_ENGINE/tsysinfo.exe" "$INSTALLPATH_KEYMANENGINE/tsysinfo.exe"
  cp *.xslt "$INSTALLPATH_KEYMANENGINE/"
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action publish:project      do_publish
builder_run_action install:project      do_install

