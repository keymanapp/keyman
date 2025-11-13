#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Keyman Setup bootstrap" \
  @/common/include \
  @/common/windows/delphi \
  clean configure build test publish debug-manifest edit

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/setup.exe"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /windows/src/desktop/setup/$WIN32_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_clean() {
  clean_windows_project_files
  rm -f Keyman.Setup.System.Locale*.pas
  rm -f icons.res
}

function do_build() {
  create-windows-output-folders
  build_version.res
  build_manifest.res
  run_in_vs_env rc icons.rc
  "$DEVTOOLS" -buildsetupstrings locale\\ .\\

  delphi_msbuild setup.dproj "//p:Platform=Win32"
  do_map2pdb "$WIN32_TARGET_PATH/setup.map" "$WIN32_TARGET"

  #
  # The installer needs to be a multiple of 512 bytes in order for the SFX code
  # to find the ZIP header. After map2pdb appends its header, the file may not
  # be the right size. So take the file size of setup.exe and generate a nul
  # file that would expand it to nearest 512 bytes, and append it to the
  # executable. This will make the file 512 bytes larger if its size is already
  # on a 512 byte boundary.
  #

  local file_size expansion_size expansion_file

  file_size=$(stat -c %s "$WIN32_TARGET")
  expansion_size=$(( 512 - ( $file_size % 512 ) ))
  expansion_file="$(mktemp)"
  dd if=/dev/zero of="${expansion_file}" bs=1 count="${expansion_size}"
  cat "$WIN32_TARGET" "$expansion_file" > "$WIN32_TARGET.out"
  mv -f "$WIN32_TARGET.out" "$WIN32_TARGET"
  rm -f "${expansion_file}"

  cp "$WIN32_TARGET" "$WINDOWS_PROGRAM_APP"
  cp_if_exists "$WIN32_TARGET_PATH/setup.pdb" "$WINDOWS_DEBUGPATH_APP"

  # setup-redist.exe does not get signed as it is intended for a bundled installer
  cp "$WIN32_TARGET" "$WINDOWS_PROGRAM_APP/setup-redist.exe"
}

function do_build_debug_manifest() {
  # make a non-elevated manifest.res for testing purposes
  rm -f manifest.res
  cp manifest.in std-manifest.tmp
  cp debug-manifest.in manifest.in
  build_manifest.res
  mv -f std-manifest.tmp manifest.in
}

function do_publish() {
  # test that (a) linked manifest exists and correct
  wrap-mt -nologo -inputresource:"$WINDOWS_PROGRAM_APP/setup.exe" -validate_manifest

  wrap-signcode //d "Keyman for Windows Setup" "$WINDOWS_PROGRAM_APP/setup.exe"
  wrap-symstore "$WINDOWS_PROGRAM_APP/setup.exe" //t keyman-windows
  wrap-symstore "$WINDOWS_DEBUGPATH_APP/setup.pdb" //t keyman-windows
}

builder_run_action clean:project        do_clean
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action publish:project      do_publish

builder_run_action debug-manifest:project  do_build_debug_manifest
builder_run_action edit:project         start setup.dproj
