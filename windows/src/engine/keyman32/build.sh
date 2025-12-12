#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Keystroke processing engine" \
  @/common/include \
  @/core:win \
  clean configure build test publish install \
  :x86 :x64 :arm64

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/keyman32.dll"
X64_TARGET="$X64_TARGET_PATH/keyman64.dll"
ARM64_TARGET="$ARM64_TARGET_PATH/keymanarm64.dll"

builder_describe_outputs \
  configure    /resources/build/win/delphi_environment_generated.inc.sh \
  build:x86    /windows/src/engine/keyman32/$WIN32_TARGET \
  build:x64    /windows/src/engine/keyman32/$X64_TARGET \
  build:arm64  /windows/src/engine/keyman32/$ARM64_TARGET \

#-------------------------------------------------------------------------------------------------------------------

function do_clean_x86() {
  vs_msbuild keyman32.vcxproj //t:Clean //p:Platform=Win32
  clean_windows_project_files
}

function do_clean_x64() {
  vs_msbuild keyman32.vcxproj //t:Clean //p:Platform=x64
  clean_windows_project_files
}

function do_clean_arm64() {
  vs_msbuild keyman32.vcxproj //t:Clean //p:Platform=arm64
  clean_windows_project_files
}

function do_configure() {
  # Install NuGet packages for keyman32.test.vcxproj
  run_in_vs_env msbuild.exe tests/keyman32.tests.vcxproj -t:Restore "-clp:Verbosity=minimal" -nologo "-p:RestorePackagesConfig=true"
  configure_windows_build_environment
}

function do_build_x86() {
  create-windows-output-folders
  build_version.res
  vs_msbuild keyman32.vcxproj //t:Build "//p:Platform=Win32"
  cp "$WIN32_TARGET" "$WINDOWS_PROGRAM_ENGINE"
  builder_if_release_build_level cp "$WIN32_TARGET_PATH/keyman32.pdb" "$WINDOWS_DEBUGPATH_ENGINE"
}

function do_build_x64() {
  create-windows-output-folders
  run_in_vs_env rc version64.rc
  vs_msbuild keyman32.vcxproj //t:Build "//p:Platform=x64"
  cp "$X64_TARGET" "$WINDOWS_PROGRAM_ENGINE"
  builder_if_release_build_level cp "$X64_TARGET_PATH/keyman64.pdb" "$WINDOWS_DEBUGPATH_ENGINE"
}

function do_build_arm64() {
  create-windows-output-folders
  run_in_vs_env rc version64.rc
  vs_msbuild keyman32.vcxproj //t:Build "//p:Platform=arm64"
  cp "$ARM64_TARGET" "$WINDOWS_PROGRAM_ENGINE"
  builder_if_release_build_level cp "$ARM64_TARGET_PATH/keymanarm64.pdb" "$WINDOWS_DEBUGPATH_ENGINE"
}

function do_publish_x86() {
  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/keyman32.dll"
  wrap-symstore "$WINDOWS_PROGRAM_ENGINE/keyman32.dll" //t keyman-engine-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_ENGINE/keyman32.pdb" //t keyman-engine-windows
}

function do_publish_x64() {
  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/keyman64.dll"
  wrap-symstore "$WINDOWS_PROGRAM_ENGINE/keyman64.dll" //t keyman-engine-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_ENGINE/keyman64.pdb" //t keyman-engine-windows
}

function do_publish_arm64() {
  wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/keymanarm64.dll"
  wrap-symstore "$WINDOWS_PROGRAM_ENGINE/keymanarm64.dll" //t keyman-engine-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_ENGINE/keymanarm64.pdb" //t keyman-engine-windows
}

function do_install_x86() {
  cp "$WINDOWS_PROGRAM_ENGINE/keyman32.dll" "$INSTALLPATH_KEYMANENGINE/keyman32.dll"
  echo "You may want to manually tweak keyman-debug-etw.man and fill in $INSTALLPATH_KEYMANENGINE/keyman32.dll"
  echo "and then run wevtutil im keyman-debug-etw.man to get the latest event tracing"
}

function do_install_x64() {
  cp "$WINDOWS_PROGRAM_ENGINE/keyman64.dll" "$INSTALLPATH_KEYMANENGINE/keyman64.dll"
}

function do_install_arm64() {
  cp "$WINDOWS_PROGRAM_ENGINE/keymanarm64.dll" "$INSTALLPATH_KEYMANENGINE/keymanarm64.dll"
}

# Parameters:
#   1: Platform = Win32 | x64
# Note Platform should not be 'x86' but rather 'Win32' for 32-bit
function do_test() {
  local Platform="$1"
  local Configuration=Release
  if builder_is_debug_build; then
    Configuration=Debug
  fi

  # We run these builds with minimal verbosity because generally any
  # environmental details that could be helpful in logs will have been revealed
  # in the build action, and it's cleaner when running testing locally to have
  # minimal logs.
  #
  # This is a non-standard build target, e.g. "Debug Static Library", so we
  # can't use our vs_msbuild wrapper here.
  run_in_vs_env --quiet msbuild.exe keyman32.vcxproj "-clp:Verbosity=minimal" -noLogo "//p:Configuration=${Configuration} Static Library" "//t:Build" "//p:Platform=${Platform}"
  # pushd tests >/dev/null
  run_in_vs_env --quiet msbuild.exe tests/keyman32.tests.vcxproj "-clp:Verbosity=minimal" -noLogo "//p:Configuration=${Configuration}" //t:Build "//p:Platform=${Platform}"
  # popd >/dev/null
  echo

  "./tests/bin/${Platform}/${Configuration}/keyman32.tests.exe"
}

builder_run_action clean:x86        do_clean_x86
builder_run_action clean:x64        do_clean_x64
builder_run_action clean:arm64      do_clean_arm64

builder_run_action configure        do_configure

builder_run_action build:x86        do_build_x86
builder_run_action build:x64        do_build_x64
builder_run_action build:arm64      do_build_arm64

builder_run_action test:x86         do_test Win32
builder_run_action test:x64         do_test x64
# Next line is currently disabled until we do processor-level checks on the executable,
# as we have no arm64 build agents yet (#15065)
# builder_run_action test:arm64       do_test arm64

builder_run_action publish:x86      do_publish_x86
builder_run_action publish:x64      do_publish_x64
builder_run_action publish:arm64    do_publish_arm64

builder_run_action install:x86      do_install_x86
builder_run_action install:x64      do_install_x64
builder_run_action install:arm64    do_install_arm64
