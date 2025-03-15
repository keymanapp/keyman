#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

builder_describe \
  "Keyman for Windows Delphi components" \
  @/common/windows/delphi \
  clean configure build test edit \
  :components \
  :message-identifiers
builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"

MESSAGE_IDENTIFIERS=/windows/src/global/delphi/cust/MessageIdentifierConsts.pas

builder_describe_outputs \
  configure:components       /resources/build/win/delphi_environment_generated.inc.sh \
  build:components           /windows/lib/keyman_components.bpl \
  build:message-identifiers  $MESSAGE_IDENTIFIERS

#-------------------------------------------------------------------------------------------------------------------

function do_build_components() {
  create-windows-output-folders
  delphi_msbuild keyman_components.dproj "//p:Platform=Win32"
  "$DEVTOOLS" -ip "$OUTLIB/keyman_components.bpl"
}

function do_build_message_identifiers() {
  # TODO: we should update devtools to accept / as well as \ in paths
  "$DEVTOOLS" -buildmessageconstants \
    "$(cygpath -w "$KEYMAN_ROOT/windows/src/desktop/kmshell/xml/strings.xml")" \
    "$(cygpath -w "$KEYMAN_ROOT/$MESSAGE_IDENTIFIERS")"
}

builder_run_action clean:components                 clean_windows_project_files
builder_run_action configure:components             configure_windows_build_environment
builder_run_action build:components                 do_build_components
builder_run_action clean:message-identifiers        rm -f "$KEYMAN_ROOT/$MESSAGE_IDENTIFIERS"
builder_run_action build:message-identifiers        do_build_message_identifiers
# builder_run_action test:project         do_test
builder_run_action edit:components                  start keyman_components.dproj
