#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
  "Keyman Developer Setup bootstrap executable" \
  @/common/include \
  @/common/windows/delphi \
  clean configure build test publish edit

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
WIN32_TARGET="$WIN32_TARGET_PATH/setup.exe"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /developer/src/setup/$WIN32_TARGET

#-------------------------------------------------------------------------------------------------------------------

function do_build() {
  create-developer-output-folders
  run_in_vs_env rc icons.rc
  build_version.res
  build_manifest.res
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


  cp "$WIN32_TARGET" "$DEVELOPER_PROGRAM"
  cp_if_exists "$WIN32_TARGET_PATH/setup.pdb" "$DEVELOPER_DEBUGPATH/devsetup.pdb"
  # builder_if_release_build_level cp "$WIN32_TARGET_PATH/setup.pdb" "$DEVELOPER_DEBUGPATH/devsetup.pdb"
  # builder_if_release_build_level mv "$WIN32_TARGET_PATH/setup.pdb" "$WIN32_TARGET_PATH/devsetup.pdb"
}

function do_publish() {
  wrap-mt -nologo -inputresource:"$DEVELOPER_PROGRAM"/setup.exe -validate_manifest
  # Not signing here because we need to sign the bundled installer instead
  # wrap-signcode /d "Keyman Developer Setup" $(DEVELOPER_PROGRAM)\setup.exe
  wrap-symstore "$DEVELOPER_PROGRAM/setup.exe" //t keyman-developer
  wrap-symstore "$DEVELOPER_DEBUGPATH/setup.pdb" //t keyman-developer
}

builder_run_action clean:project        clean_windows_project_files
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
builder_run_action publish:project      do_publish
# builder_run_action test:project         do_test
builder_run_action edit:project         start setup.dproj
