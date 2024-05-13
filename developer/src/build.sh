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

builder_parse "$@"

# builder_describe_outputs \
#   configure  /developer/src/tike/xml/layoutbuilder/keymanweb-osk.ttf
# builder_run_action configure cp "$KEYMAN_ROOT/common/resources/fonts/keymanweb-osk.ttf" "$KEYMAN_ROOT/developer/src/tike/xml/layoutbuilder/"

if builder_has_action publish; then
  # Make sure that package*.json have not been modified before
  pushd "$KEYMAN_ROOT" >/dev/null
  if git status --porcelain | grep -qP 'package(-lock)?\.json'; then
    builder_echo "The following package.json files have been modified:"
    git status --porcelain | grep -P 'package(-lock)?\.json'
    builder_die "The publish action will not run until these files are checked in or reverted."
  fi
  popd

  # To ensure that we cache the top-level package.json, we must call this before
  # the global publish
  builder_publish_cleanup
fi

builder_run_child_actions clean configure build test api publish install

if builder_has_action publish; then
  builder_echo info "Cleaning up package.json after 'npm version'"
  # And then cleanup the mess
  builder_publish_cleanup
  # Restore all the package.json files and package-lock.json files that
  # were clobbered by 'npm version'
  pushd "$KEYMAN_ROOT" >/dev/null
  git checkout package.json package-lock.json '**/package.json'
  popd
fi

function do_api() {
  api-documenter markdown -i ../build/api -o ../build/docs
  # TODO: Copy to help.keyman.com and open PR {in publish step}
}

builder_run_action api do_api
