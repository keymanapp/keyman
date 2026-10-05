#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Text Services Framework Text Input Processor" \
  @/common/include \
  @/windows/src/engine/keymanmc \
  clean configure build test publish install

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/inst-cpp.dll"
X64_TARGET="$X64_TARGET_PATH/inst-cpp64.dll"
# ARM64EC_TARGET="$ARM64EC_TARGET_PATH/inst-cpparm64x.dll"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:x86    /windows/src/engine/inst-cpp/$WIN32_TARGET \
  build:x64    /windows/src/engine/inst-cpp/$X64_TARGET \
  # build:arm64ec  /windows/src/engine/inst-cpp/$ARM64EC_TARGET \

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  vs_msbuild inst-cpp.vcxproj //t:Clean
  clean_windows_project_files
}

function do_build() {
  create-windows-output-folders
  build_version.res
  vs_msbuild inst-cpp.vcxproj //t:Build "//p:Platform=Win32"
  vs_msbuild inst-cpp.vcxproj //t:Build "//p:Platform=x64"
  # vs_msbuild inst-cpp.vcxproj //t:Build "//p:Platform=arm64EC"
  cp "$WIN32_TARGET" "$WINDOWS_PROGRAM_ENGINE"
  builder_if_release_build_level cp "$WIN32_TARGET_PATH/inst-cpp.pdb" "$WINDOWS_DEBUGPATH_ENGINE"
  cp "$X64_TARGET" "$WINDOWS_PROGRAM_ENGINE"
  builder_if_release_build_level cp "$X64_TARGET_PATH/inst-cpp64.pdb" "$WINDOWS_DEBUGPATH_ENGINE"
  # cp "$ARM64EC_TARGET" "$WINDOWS_PROGRAM_ENGINE"
  # builder_if_release_build_level cp "$ARM64EC_TARGET_PATH/inst-cpparm64x.pdb" "$WINDOWS_DEBUGPATH_ENGINE"

}

function do_publish() {
  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/inst-cpp.dll"
  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/inst-cpp64.dll"
  # wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/inst-cpparm64x.dll"
  wrap-symstore "$WINDOWS_PROGRAM_ENGINE/inst-cpp.dll" //t keyman-engine-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_ENGINE/inst-cpp.pdb" //t keyman-engine-windows
  wrap-symstore "$WINDOWS_PROGRAM_ENGINE/inst-cpp64.dll" //t keyman-engine-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_ENGINE/inst-cpp64.pdb" //t keyman-engine-windows
  # wrap-symstore "$WINDOWS_PROGRAM_ENGINE/inst-cpparm64x.dll" //t keyman-engine-windows
  # wrap-symstore "$WINDOWS_DEBUGPATH_ENGINE/inst-cpparm64x.pdb" //t keyman-engine-windows
}

function do_install() {
  cp "$WINDOWS_PROGRAM_ENGINE/inst-cpp.dll" "$INSTALLPATH_KEYMANENGINE/inst-cpp.dll"
  cp "$WINDOWS_PROGRAM_ENGINE/inst-cpp64.dll" "$INSTALLPATH_KEYMANENGINE/inst-cpp64.dll"
  # cp "$WINDOWS_PROGRAM_ENGINE/inst-cpparm64x.dll" "$INSTALLPATH_KEYMANENGINE/inst-cpparm64x.dll"
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action publish:project      do_publish
builder_run_action install:project      do_install
