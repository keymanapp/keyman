#!/usr/bin/env bash
# shellcheck disable=SC2154 # (variables are set in build-utils.sh)
# Actions for creating a Debian source package and verifying the API.
# Used by deb-packaging.yml and api-verification.yml GHAs.

set -eu
shopt -s inherit_errexit

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "${KEYMAN_ROOT}/linux/scripts/verify_api.inc.sh"

builder_describe \
  "Helper for building Debian packages." \
  "dependencies               Install dependencies as found in debian/control" \
  "source+                    Build source package" \
  "verify                     Verify API" \
  "--gha                      Build from GitHub Action" \
  "--bin-pkg=BIN_PKG          Path and name of binary Debian package (for verify action)" \
  "--git-sha=GIT_SHA          The SHA of the HEAD commit, e.g. of the PR branch (for verify action)" \
  "--git-base=GIT_BASE        The ref of the base commit, e.g. usually the name of the base" \
  "                           branch, e.g. master, stable-16.0 (for verify action)"

builder_parse "$@"

cd "${REPO_ROOT}/linux"

if builder_has_option --gha; then
  START_STEP="::group::${COLOR_GREEN}"
  END_STEP="::endgroup::"
else
  START_STEP="${COLOR_GREEN}"
  END_STEP=""
fi

dependencies_action() {
  sudo mk-build-deps --install --tool='apt-get -o Debug::pkgProblemResolver=yes --no-install-recommends --yes' debian/control
  # Additionally we need quilt to be able to create the source package.
  # Since this is not needed to build the binary package, it is not
  # (and should not be) included in `build-depends` in `debian/control`.
  sudo DEBIAN_FRONTEND=noninteractive apt-get -q -y install quilt
}

source_action() {
  echo "${START_STEP}Make source package for keyman${COLOR_RESET}"

  echo "${START_STEP}reconfigure${COLOR_RESET}"
  ./scripts/reconf.sh
  echo "${END_STEP}"

  echo "${START_STEP}Make origdist${COLOR_RESET}"
  ./scripts/dist.sh origdist
  echo "${END_STEP}"

  echo "${START_STEP}Make deb source${COLOR_RESET}"
  ./scripts/deb.sh
  echo "${END_STEP}"

  mv builddebs/* "${OUTPUT_PATH:-..}"
}

builder_run_action dependencies  dependencies_action
builder_run_action source        source_action
builder_run_action verify        verify_api_action
