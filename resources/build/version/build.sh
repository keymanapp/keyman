#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../resources/build/builder-full.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"
. "$KEYMAN_ROOT/resources/build/node.inc.sh"

################################ Main script ################################

# assuming that last stable release is also minor version .0 at this point
STABLE_VERSION_MINOR=0
STABLE_VERSION_MAJOR=$(( $KEYMAN_VERSION_MAJOR - 1 ))

builder_describe "Build version tooling" clean configure build \
  "report-stable-changes   Report on pending changes to most recent Keyman stable-$STABLE_VERSION_MAJOR.$STABLE_VERSION_MINOR release"

builder_describe_outputs \
  configure /resources/build/version/node_modules \
  build     /resources/build/version/build/src/index.js

builder_describe_internal_dependency \
  report-stable-changes:project  build:project

builder_parse "$@"

function do_report_stable_changes() {
  cd "$KEYMAN_ROOT"
  git fetch
  node resources/build/version report-history -b "stable-$STABLE_VERSION_MAJOR.$STABLE_VERSION_MINOR" -t "$GITHUB_TOKEN" --github-pr
}

builder_run_action clean      rm -rf build/ node_modules/ dist/ lib/
builder_run_action configure  node_select_version_and_npm_ci
builder_run_action build      tsc --build
builder_run_action report-stable-changes        do_report_stable_changes