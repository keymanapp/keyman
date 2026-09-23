#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"

builder_describe \
  "Keyman Developer" \
  "@/resources/tools/check-markdown  test:help" \
  clean \
  configure \
  build \
  test \
  "api                          Analyze API and prepare API documentation" \
  "publish                      Prepare files for distribution, publish symbols, and build installer" \
  "install                      Install built programs locally" \
  ":common                      Developer common files" \
  ":ext                         Third party components" \
  ":help                        Online documentation" \
  ":kmcmplib                    Compiler - .kmn compiler" \
  ":kmc-analyze                 Compiler - Analysis Tools" \
  ":kmc-copy                    Compiler - Project Copying and Renaming Tools" \
  ":kmc-convert                 Compiler - Keyboard Conversion Tools" \
  ":kmc-generate                Compiler - Generation Tools" \
  ":kmc-keyboard-info           Compiler - .keyboard_info Module" \
  ":kmc-kmn                     Compiler - .kmn to .kmx and .js Keyboard Module" \
  ":kmc-ldml                    Compiler - LDML Keyboard Module" \
  ":kmc-model                   Compiler - Lexical Model Module" \
  ":kmc-model-info              Compiler - .model_info Module" \
  ":kmc-package                 Compiler - Package Module" \
  ":kmc                         Compiler - Command Line Interface" \
  ":kmanalyze                   Legacy keyboard analysis tool" \
  ":kmdbrowserhost              Keyman Developer Browser Host" \
  ":kmdecomp                    Unsupported decompiler" \
  ":kmconvert                   Legacy keyboard generation and conversion tool" \
  ":samples                     Sample projects" \
  ":server                      Keyman Developer Server" \
  ":setup                       Keyman Developer setup bootstrap" \
  ":test=test/auto              Various older tests (others in each module)" \
  ":tike                        Keyman Developer IDE" \
  ":inst                        Bundled installers"

builder_describe_platform \
  :ext       win,delphi \
  :kmanalyze win \
  :kmdbrowserhost win,delphi \
  :kmdecomp  win \
  :kmconvert win,delphi \
  :samples   win \
  :server    win \
  :setup     win,delphi \
  :test      win,delphi \
  :tike      win,delphi \
  :inst      win,delphi

# TODO: in future :server could be built on other platforms, potentially, but it
# has addons that are Windows-specific currently

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

if builder_is_windows; then
  source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
fi

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
    "$KEYMAN_ROOT"/common/windows/delphi/tools/devtools/build.sh configure build
  fi

  "$KEYMAN_ROOT"/common/windows/delphi/tools/devtools/build.sh prepublish

  #
  # Verify that klog is disabled
  #

  if [[ ! -f /common/windows/delphi/tools/test-klog/$WIN32_TARGET_PATH/test_klog.exe ]]; then
    # We'll build test_klog here directly because we are before the configure /
    # build steps which would trigger it in via dependencies
    "$KEYMAN_ROOT"/common/windows/delphi/tools/test-klog/build.sh configure build
  fi

  "$KEYMAN_ROOT"/common/windows/delphi/tools/test-klog/build.sh prepublish

  # All prepublish steps finished
}

if builder_has_action publish; then
  do_prepublish
fi

#-------------------------------------------------------------------------------------------------------------------

builder_run_child_actions  clean configure build test
builder_run_action         test:help    check-markdown  "$KEYMAN_ROOT/developer/docs/help"

#-------------------------------------------------------------------------------------------------------------------

builder_run_child_actions  api
builder_run_action         api     api-documenter markdown -i ../build/api -o ../build/docs

#-------------------------------------------------------------------------------------------------------------------

builder_run_child_actions  publish

#-------------------------------------------------------------------------------------------------------------------

builder_run_child_actions  install
