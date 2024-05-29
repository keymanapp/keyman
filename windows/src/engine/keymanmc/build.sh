#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe "Message library for Event Viewer" \
  clean configure build test publish install

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"

builder_describe_outputs \
  configure:project    /resources/build/win/delphi_environment_generated.inc.sh \
  build:project        /windows/src/engine/keymanmc/keymanmc.dll

#-------------------------------------------------------------------------------------------------------------------

function do_build() {
  create-windows-output-folders
  run_in_vs_env mc -U keymanmc.mc
  run_in_vs_env rc keymanmc.rc
  run_in_vs_env link -dll -noentry keymanmc.res -machine:x86
  cp keymanmc.dll "$WINDOWS_PROGRAM_ENGINE"
}

function do_install() {
  cp "$WINDOWS_PROGRAM_ENGINE/keymanmc.dll" "$INSTALLPATH_KEYMANENGINE/keymanmc.dll"
  reg add 'HKLM\\SYSTEM\\CurrentControlSet\\services\\eventlog\\Application\\Keyman' //v CategoryCount //t REG_DWORD //d 1 //f
  reg add 'HKLM\\SYSTEM\\CurrentControlSet\\services\\eventlog\\Application\\Keyman' //v CategoryMessageFile //t REG_SZ //d "$INSTALLPATH_KEYMANENGINE\\keymanmc.dll" //f
  reg add 'HKLM\\SYSTEM\\CurrentControlSet\\services\\eventlog\\Application\\Keyman' //v EventMessageFile //t REG_SZ //d "$INSTALLPATH_KEYMANENGINE\\keymanmc.dll" //f
  reg add 'HKLM\\SYSTEM\\CurrentControlSet\\services\\eventlog\\Application\\Keyman' //v TypesSupported //t REG_DWORD //d 7 //f
}

builder_run_action clean:project        rm -f keymanmc.dll keymanmc.h keymanmc.rc keymanmc.res MSG00409.bin
builder_run_action configure:project    configure_windows_build_environment
builder_run_action build:project        do_build
# builder_run_action test:project         do_test
builder_run_action publish:project      wrap-signcode //d "Keyman Engine for Windows" "$WINDOWS_PROGRAM_ENGINE/keymanmc.dll"
builder_run_action install:project      do_install
