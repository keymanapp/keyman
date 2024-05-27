#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Diagnostics (64-bit)" \
  @/common/include \
  @/common/windows/delphi \
  clean configure build test publish install

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN64_TARGET="$WIN64_TARGET_PATH/tsysinfox64.exe"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /windows/src/engine/tsysinfox64/$WIN64_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_build() {
  create-windows-output-folders
  build_version.res
  build_manifest.res

  delphi_msbuild tsysinfox64.dproj "//p:Platform=Win64"
  cp "$WIN64_TARGET" "$WINDOWS_PROGRAM_ENGINE"
}

function do_publish() {
  # test that (a) linked manifest exists and correct
  wrap-mt -nologo -inputresource:"$WINDOWS_PROGRAM_ENGINE/tsysinfox64.exe" -validate_manifest

  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/tsysinfox64.exe"
}

function do_install() {
  cp "$WINDOWS_PROGRAM_ENGINE/tsysinfox64.exe" "$INSTALLPATH_KEYMANENGINE/tsysinfox64.exe"
  cp *.xslt "$INSTALLPATH_KEYMANENGINE/"
}

builder_run_action clean:project        clean_windows_project_files
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action publish:project      do_publish
builder_run_action install:project      do_install

