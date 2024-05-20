#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"
. "$KEYMAN_ROOT/developer/src/packages.inc.sh"

builder_describe \
  "Keyman for Windows (app)" \
  \
  clean \
  configure \
  build \
  test \
  "publish                      Prepare files for distribution, publish symbols, and build installer" \
  "install                      Install built programs locally" \
  \
  ":help                        Help documentation" \
  ":kmbrowserhost               Chromium browser host process" \
  ":kmconfig                    Low level configuration tool" \
  ":kmshell                     Keyman Configuration" \
  ":setup                       Keyman for Windows setup bootstrap" \
  ":insthelp                    Installation helper tool" \
  ":inst                        Bundled installers"

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"

#
# We want to do some checks before we head down the long publish path
#
function do_prepublish() {
  builder_heading "prepublish - verify environment before build"

  #
  # Verify that the Delphi environment is correct for a release build
  #

  if [[ ! -f "$DEVTOOLS" ]]; then
    # We'll build devtools here directly because we are before the configure /
    # build steps which would trigger it in via dependencies
    "$KEYMAN_ROOT"/common/windows/delphi/tools/devtools/build.sh configure build prepublish
  fi

  #
  # Verify that klog is disabled
  #

  if [[ ! -f /common/windows/delphi/tools/test-klog/$WIN32_TARGET_PATH/test_klog.exe ]]; then
    # We'll build test_klog here directly because we are before the configure /
    # build steps which would trigger it in via dependencies
    "$KEYMAN_ROOT"/common/windows/delphi/tools/test-klog/build.sh configure build prepublish
  fi

  "$DEVTOOLS" -rt

  # All prepublish steps finished
}

if builder_has_action publish; then
  do_prepublish
fi

#-------------------------------------------------------------------------------------------------------------------

builder_run_child_actions  clean configure build test publish install
