#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Build Keyman Developer IDE" \
  @/core:x86 \
  @/common/windows/delphi \
  clean configure build test publish install

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/tike.exe"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /developer/src/tike/$WIN32_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  rm -rf bin obj manifest.res manifest.xml *.dproj.local version.res icons.RES icons.res *.identcache icons.res
}

function do_configure() {
  configure_windows_build_environment
  do_monaco_copy

  mkdir -p "$DEVELOPER_PROGRAM"
  cp "$KEYMAN_ROOT/common/schemas/kps/kps.xsd" "$DEVELOPER_PROGRAM"
  cp "$KEYMAN_ROOT/common/resources/fonts/keymanweb-osk.ttf" "$DEVELOPER_ROOT/src/tike/xml/layoutbuilder/keymanweb-osk.ttf"
}

function do_monaco_copy() {
  #
  # Grab the version of Monaco and any other node modules
  # that we want to use in Keyman Developer, and copy into
  # lib folder for sane paths. If we use more npm modules in the
  # future, we may consolidate the paths at that time.
  #
  pushd "$DEVELOPER_ROOT/src/tike/xml/app"
  npm ci
  popd
  rm -rf "$DEVELOPER_ROOT/src/tike/xml/app/lib/monaco"
  mkdir -p "$DEVELOPER_ROOT/src/tike/xml/app/lib/monaco/min"
  mkdir -p "$DEVELOPER_ROOT/src/tike/xml/app/lib/monaco/min-maps"
  cp -R "$DEVELOPER_ROOT/src/tike/xml/app/node_modules/monaco-editor/min"/* "$DEVELOPER_ROOT/src/tike/xml/app/lib/monaco/min"
  cp -R "$DEVELOPER_ROOT/src/tike/xml/app/node_modules/monaco-editor/min-maps"/* "$DEVELOPER_ROOT/src/tike/xml/app/lib/monaco/min-maps"

  pushd "$DEVELOPER_ROOT/src/tike/xml/app/lib/sentry"
  replaceVersionStrings_Mkver init.js.in init.js
  popd

  run_in_vs_env rc icons.rc
}

function do_build() {
  create-developer-output-folders
  build_version.res
  build_manifest.res

  rm -rf "$DEVELOPER_PROGRAM/xml"
  mkdir -p "$DEVELOPER_PROGRAM/xml"
  cp -R "$DEVELOPER_ROOT/src/tike/xml/"* "$DEVELOPER_PROGRAM/xml/"

  delphi_msbuild tike.dproj "//p:Platform=Win32"
  sentrytool_delphiprep "$WIN32_TARGET" tike.dpr
  tds2dbg "$WIN32_TARGET"

  cp "$WIN32_TARGET" "$DEVELOPER_PROGRAM"
  cp kmlmc.cmd "$DEVELOPER_PROGRAM"
  cp kmlmp.cmd "$DEVELOPER_PROGRAM"
  cp kmc.cmd "$DEVELOPER_PROGRAM"
  cp "$KEYMAN_ROOT/core/build/x86/$TARGET_PATH/src/keymancore-1.dll" "$DEVELOPER_PROGRAM"
  if [[ -f "$WIN32_TARGET_PATH/tike.dbg" ]]; then
    cp "$WIN32_TARGET_PATH/tike.dbg" "$DEVELOPER_DEBUGPATH"
  fi
  cp "$KEYMAN_ROOT/core/build/x86/$TARGET_PATH/src/keymancore-1.dll" "$WIN32_TARGET_PATH"
  cp "$KEYMAN_ROOT/core/build/x86/$TARGET_PATH/src/keymancore-1.pdb" "$WIN32_TARGET_PATH"

  cp "$KEYMAN_ROOT/common/windows/delphi/ext/sentry/sentry.dll" "$DEVELOPER_PROGRAM/"
  cp "$KEYMAN_ROOT/common/windows/delphi/ext/sentry/sentry.x64.dll" "$DEVELOPER_PROGRAM/"
  cp "$KEYMAN_ROOT/common/windows/delphi/ext/sentry/crashpad_handler.exe" "$DEVELOPER_PROGRAM/"

}

function do_publish() {
  # test that (a) linked manifest exists and correct
  wrap-mt -nologo -inputresource:"$DEVELOPER_PROGRAM"/tike.exe -validate_manifest

  wrap-signcode //d "Keyman Developer" "$DEVELOPER_PROGRAM/tike.exe"
  wrap-signcode //d "Keyman Core" "$DEVELOPER_PROGRAM/keymancore-1.dll"
  # Sign the Sentry executables and libraries here
  wrap-signcode //d "Keyman Developer" "$DEVELOPER_PROGRAM/sentry.dll"
  wrap-signcode //d "Keyman Developer" "$DEVELOPER_PROGRAM/sentry.x64.dll"
  wrap-signcode //d "Keyman Developer" "$DEVELOPER_PROGRAM/crashpad_handler.exe"
  wrap-symstore "$DEVELOPER_PROGRAM/tike.exe" //t keyman-developer
  wrap-symstore "$DEVELOPER_DEBUGPATH/tike.dbg" //t keyman-developer
}

function do_install() {
  cp "$DEVELOPER_PROGRAM/tike.exe" "$INSTALLPATH_KEYMANDEVELOPER/tike.exe"
  cp "$DEVELOPER_PROGRAM/keymancore-1.dll" "$INSTALLPATH_KEYMANDEVELOPER/keymancore-1.dll"
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    do_configure
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action publish:project      do_publish
builder_run_action install:project      do_install

# Note: generating monaco installer:
# @echo *******************************************************************************************
# @echo The generated ..\inst\monaco_gen.wxs file will require manual merging with the existing
# @echo monaco.xws. Existing component GUIDs must be maintained per Windows Installer component
# @echo rules (a file at a single location should have a permanent GUID for reference counting).
# @echo *******************************************************************************************
# $(WIXPATH)\heat.exe dir xml\app\lib\monaco\min\vs -gg -o ..\inst\monaco_gen.wxs -cg MonacoFiles -var var.MonacoSourceDir -dr MonacoTargetDir
