#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Keyman main host process (64-bit)" \
  @/common/include \
  clean configure build test publish install \
  :x64 :arm64

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
X64_TARGET="$X64_TARGET_PATH/keymanhp.x64.exe"
ARM64_TARGET="$ARM64_TARGET_PATH/keymanhp.arm64.exe"

builder_describe_outputs \
  configure    /resources/build/win/delphi_environment_generated.inc.sh \
  build:x64        /windows/src/engine/keymanhp/$X64_TARGET \
  build:arm64      /windows/src/engine/keymanhp/$ARM64_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_clean_x64() {
  vs_msbuild keymanhp.vcxproj //t:Clean //p:Platform=x64
  clean_windows_project_files
}

function do_clean_arm64() {
  vs_msbuild keymanhp.vcxproj //t:Clean //p:Platform=arm64
  clean_windows_project_files
}

function do_build_x64() {
  create-windows-output-folders
  run_in_vs_env rc versionx64.rc
  sed "s/Keyman Engine HP/Keyman Engine x64/g;
       s/arch64/amd64/g" manifesthp.in > manifest.in
  build_manifest.res
  vs_msbuild keymanhp.vcxproj //t:Build "//p:Platform=x64"
  cp "$X64_TARGET" "$WINDOWS_PROGRAM_ENGINE"
  builder_if_release_build_level cp "$X64_TARGET_PATH/keymanhp.x64.pdb" "$WINDOWS_DEBUGPATH_ENGINE"
  rm manifest.in
}

function do_build_arm64() {
  create-windows-output-folders
  run_in_vs_env rc versionarm64.rc
  sed "s/Keyman Engine HP/Keyman Engine arm64/g;
       s/arch64/arm64/g" manifesthp.in > manifest.in
  build_manifest.res
  vs_msbuild keymanhp.vcxproj //t:Build "//p:Platform=arm64"
  cp "$ARM64_TARGET" "$WINDOWS_PROGRAM_ENGINE"
  builder_if_release_build_level cp "$ARM64_TARGET_PATH/keymanhp.arm64.pdb" "$WINDOWS_DEBUGPATH_ENGINE"
  rm manifest.in
}

function do_publish_x64() {
  wrap-mt -nologo -inputresource:"$WINDOWS_PROGRAM_ENGINE/keymanhp.x64.exe" -validate_manifest
  wrap-mt -nologo -inputresource:"$WINDOWS_PROGRAM_ENGINE/keymanhp.x64.exe" -out:temp.manifest
  grep -q 'uiAccess="true"' temp.manifest || builder_die 'uiAccess must be set to true in manifest.xml'
  rm -f temp.manifest

  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/keymanhp.x64.exe"
  wrap-symstore "$WINDOWS_PROGRAM_ENGINE/keymanhp.x64.exe" //t keyman-engine-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_ENGINE/keymanhp.x64.pdb" //t keyman-engine-windows
}

function do_publish_arm64() {
  wrap-mt -nologo -inputresource:"$WINDOWS_PROGRAM_ENGINE/keymanhp.arm64.exe" -validate_manifest
  wrap-mt -nologo -inputresource:"$WINDOWS_PROGRAM_ENGINE/keymanhp.arm64.exe" -out:temp.manifest
  grep -q 'uiAccess="true"' temp.manifest || builder_die 'uiAccess must be set to true in manifest.xml'
  rm -f temp.manifest

  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/keymanhp.arm64.exe"
  wrap-symstore "$WINDOWS_PROGRAM_ENGINE/keymanhp.arm64.exe" //t keyman-engine-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_ENGINE/keymanhp.arm64.pdb" //t keyman-engine-windows
}

function do_install_x64() {
  cp "$WINDOWS_PROGRAM_ENGINE/keymanhp.x64.exe" "$INSTALLPATH_KEYMANENGINE/keymanhp.x64.exe"
}

function do_install_arm64() {
  cp "$WINDOWS_PROGRAM_ENGINE/keymanhp.arm64.exe" "$INSTALLPATH_KEYMANENGINE/keymanhp.arm64.exe"
}


builder_run_action clean:x64        do_clean_x64
builder_run_action clean:arm64      do_clean_arm64
builder_run_action configure        configure_windows_build_environment
builder_run_action build:x64        do_build_x64
builder_run_action build:arm64      do_build_arm64
# builder_run_action test:project         do_test
builder_run_action publish:x64      do_publish_x64
builder_run_action publish:arm64    do_publish_arm64
builder_run_action install:x64      do_install_x64
builder_run_action install:arm64    do_install_arm64
