#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"
. "$KEYMAN_ROOT/resources/build/build-utils-ci.inc.sh"
. "$KEYMAN_ROOT/developer/src/packages.inc.sh"

builder_describe \
  "Keyman Developer" \
  clean \
  configure \
  build \
  test \
  "api                          Analyze API and prepare API documentation" \
  "publish                      Prepare files for distribution, publish symbols, publish or pack npm packages, and build installer" \
  "install                      Install built programs locally" \
  ":common                      Developer common files" \
  ":ext                         Third party components" \
  ":kmcmplib                    Compiler - .kmn compiler" \
  ":kmc-analyze                 Compiler - Analysis Tools" \
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
  ":inst                        Bundled installers" \
  "--npm-publish+               For publish, do a npm publish, not npm pack (only for CI)" \
  "--dry-run,-n+                Don't actually publish anything to external endpoints, just dry run"

builder_describe_platform \
  :ext       win,delphi \
  :kmanalyze win \
  :kmdbrowserhost win,delphi \
  :kmdecomp  win \
  :kmconvert win,delphi \
  :samples   win \
  :setup     win,delphi \
  :test      win,delphi \
  :tike      win,delphi \
  :inst      win,delphi

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

if [[ $BUILDER_OS == win ]]; then
  source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
fi

#
# We want to do some checks before we head down the long publish path
#
function do_prepublish() {
  builder_heading "prepublish - verify environment before build"

  #
  # Make sure that package*.json have not been modified before
  #

  pushd "$KEYMAN_ROOT" >/dev/null
  if git status --porcelain | grep -qP 'package(-lock)?\.json'; then
    builder_echo "The following package.json files have been modified:"
    git status --porcelain | grep -P 'package(-lock)?\.json'
    builder_die "The publish action will not run until these files are checked in or reverted."
  fi
  popd

  #
  # To ensure that we cache the top-level package.json, we must call this before
  # the global publish
  #

  builder_publish_cleanup

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

#-------------------------------------------------------------------------------------------------------------------

builder_run_child_actions  api
builder_run_action         api     api-documenter markdown -i ../build/api -o ../build/docs

#-------------------------------------------------------------------------------------------------------------------

function do_publish() {
  #--------------------------------------------------------
  # TODO: Hard-coded calls to /common packages which need
  # publishing; this should be able to be removed once we
  # move the publish call to the top-level

  local DRY_RUN= NPM_PUBLISH=

  if builder_has_option --npm-publish; then
    NPM_PUBLISH=--npm-publish
  fi

  if builder_has_option --dry-run; then
    DRY_RUN=--dry-run
  fi

  ../../common/web/keyman-version/build.sh publish $DRY_RUN $NPM_PUBLISH
  ../../common/web/types/build.sh publish $DRY_RUN $NPM_PUBLISH
  ../../common/models/types/build.sh publish $DRY_RUN $NPM_PUBLISH
  ../../core/include/ldml/build.sh publish $DRY_RUN $NPM_PUBLISH
  # end TODO
  #--------------------------------------------------------

  builder_echo info "Cleaning up package.json after 'npm version'"
  # And then cleanup the mess
  builder_publish_cleanup
  # Restore all the package.json files and package-lock.json files that
  # were clobbered by 'npm version'
  pushd "$KEYMAN_ROOT" >/dev/null
  git checkout package.json package-lock.json '**/package.json'
  popd

  # TODO: Copy ../build/docs {from api action} to help.keyman.com and open PR
}

builder_run_child_actions  publish
builder_run_action         publish do_publish

#-------------------------------------------------------------------------------------------------------------------

builder_run_child_actions  install
